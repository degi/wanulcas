import os

filepath = r"c:\Degi\GitHub\wanulcas\docs\w_notes.md"
with open(filepath, "r", encoding="utf-8") as f:
    content = f.read()

blocks = content.split('\n\n')
new_blocks = []

in_code = False

for block in blocks:
    if block.startswith('```'):
        new_blocks.append(block)
        # Toggle in_code if it's the start/end of a code block.
        # But wait, a single block could be ```r\ncode\n```, in which case it doesn't span across '\n\n'.
        # Actually, let's just do a simpler code block check:
        continue

    # Skip lists, tables, headers, blockquotes, images
    if block.lstrip().startswith(('#', '|', '>', '-', '*', '!')):
        new_blocks.append(block)
        continue
        
    lines = block.split('\n')
    joined = lines[0]
    for line in lines[1:]:
        if joined.endswith(' '):
            joined += line.lstrip()
        else:
            joined += ' ' + line.lstrip()
    
    new_blocks.append(joined)

with open(filepath, "w", encoding="utf-8") as f:
    f.write('\n\n'.join(new_blocks))

print("Unwrapped paragraphs.")
