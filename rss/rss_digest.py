#!/usr/bin/env python3
import json
import subprocess
import re
import os
import sys
import html
from urllib.parse import urlparse
import email.utils
import datetime

def get_emails():
    """Retrieves emails from notmuch matching the query."""
    query = "folder:rss and tag:inbox"
    try:
        # --format=json gives us structured data
        # --include-html=false ensures we mostly get text parts to search for URLs
        result = subprocess.run(
            ["notmuch", "show", "--format=json", "--body=true", query],
            capture_output=True,
            text=True,
            check=True
        )
        return json.loads(result.stdout)
    except subprocess.CalledProcessError as e:
        print(f"Error running notmuch: {e}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"Error parsing notmuch output: {e}")
        sys.exit(1)

def extract_urls(content):
    """Finds all http/https URLs in the text content."""
    # Basic regex to find URLs.
    url_pattern = re.compile(r'(https?://[^\s<>"]+)')
    return url_pattern.findall(content)

def find_content_recursive(parts):
    """Recursively searches for text/plain content in message parts."""
    text_content = ""
    if isinstance(parts, list):
        for part in parts:
            text_content += find_content_recursive(part)
    elif isinstance(parts, dict):
        if parts.get("content-type") == "text/plain":
            content = parts.get("content")
            if content:
                text_content += content + "\n"
        elif "content" in parts:
             text_content += find_content_recursive(parts["content"])
    return text_content

def main():
    threads = get_emails()
    
    items = []

    for thread in threads:
        # Threads are lists of lists of ... messages (tree structure)
        # We need to flatten or traverse this. 
        # The top level structure from 'notmuch show' is a list of threads.
        # Each thread is a list containing the thread structure.
        
        def traverse_thread(node):
            if isinstance(node, list):
                for item in node:
                    traverse_thread(item)
            elif isinstance(node, dict):
                # This is a message or part of the structure
                if "headers" in node:
                    subject = node["headers"].get("Subject", "No Subject")
                    date_str = node["headers"].get("Date", "")
                    
                    try:
                        dt = email.utils.parsedate_to_datetime(date_str)
                    except (TypeError, ValueError):
                         # Fallback for parsing errors, use current time or min time
                        dt = datetime.datetime.min.replace(tzinfo=datetime.timezone.utc)

                    # Try to get URL from body
                    body_content = ""
                    if "body" in node:
                        body_content = find_content_recursive(node["body"])
                    
                    urls = extract_urls(body_content)
                    
                    # Deduplicate URLs while preserving order
                    unique_urls = []
                    seen = set()
                    for url in urls:
                        if url not in seen:
                            unique_urls.append(url)
                            seen.add(url)

                    if unique_urls:
                        items.append({
                            "subject": subject, 
                            "urls": unique_urls,
                            "date": dt
                        })
                
                # Check for replies/children
                if "body" in node and isinstance(node["body"], list):
                     traverse_thread(node["body"])

        traverse_thread(thread)

    # Sort items by date, newest first
    items.sort(key=lambda x: x["date"], reverse=True)

    output_path = os.path.expanduser("~/Downloads/daily.html")
    
    html_lines = [
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "    <title>Daily RSS Digest</title>",
        "    <style>",
        "        body { font-family: sans-serif; max-width: 900px; margin: 2rem auto; line-height: 1.6; }",
        "        ul { list-style-type: none; padding: 0; }",
        "        li { margin-bottom: 1rem; border-bottom: 1px solid #eee; padding-bottom: 0.5rem; display: flex; align-items: baseline; flex-wrap: wrap; }",
        "        .date { font-family: monospace; color: #666; font-size: 0.9rem; margin-right: 1rem; white-space: nowrap; }",
        "        .subject { font-weight: bold; font-size: 1.1rem; margin-right: 0.5rem; }",
        "        .links { display: inline-block; }",
        "        a { text-decoration: none; color: #0066cc; font-size: 0.9rem; margin-left: 0.3rem; }",
        "        a:hover { text-decoration: underline; }",
        "    </style>",
        "</head>",
        "<body>",
        "    <h1>Daily RSS Digest</h1>",
        "    <ul>"
    ]
    
    html_content = "\n".join(html_lines) + "\n"
    
    if not items:
        html_content += "<li>No new RSS items found.</li>"
    else:
        for item in items:
            # Format date for display
            display_date = item["date"].strftime("%Y-%m-%d %H:%M")
            
            html_content += f'        <li><span class="date">{display_date}</span><span class="subject">{html.escape(item["subject"])}</span><span class="links">'
            for url in item["urls"]:
                try:
                    parsed = urlparse(url)
                    domain = parsed.netloc
                    if domain.startswith("www."):
                        domain = domain[4:]
                    if not domain:
                         domain = "link"
                except:
                    domain = "link"
                
                html_content += f'<a href="{html.escape(url)}" target="_blank">[{html.escape(domain)}]</a>'
            html_content += '</span></li>\n'

    html_content += "    </ul>\n</body>\n</html>\n"

    try:
        with open(output_path, "w") as f:
            f.write(html_content)
        print(f"Generated {output_path}")
    except IOError as e:
        print(f"Error writing to file: {e}")
        sys.exit(1)

    # Open in Firefox
    firefox_cmd = [
        "flatpak", "run", "--env=MOZ_DBUS_REMOTE=1",
        "org.mozilla.firefox", "--new-tab", f"file://{output_path}"
    ]
    
    print(f"Opening in Firefox...")
    try:
        subprocess.run(firefox_cmd, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error launching Firefox: {e}")

if __name__ == "__main__":
    main()
