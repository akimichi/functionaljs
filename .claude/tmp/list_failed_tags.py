#!/usr/bin/env python3
"""
失敗したタグのリストを出力
"""
import re
from pathlib import Path

def find_missing_tags(js_path, ts_path):
    """JS内のタグでTSに存在しないものを見つける"""
    with open(js_path, 'r', encoding='utf-8') as f:
        js_content = f.read()
    with open(ts_path, 'r', encoding='utf-8') as f:
        ts_content = f.read()

    # JSからタグ名を抽出
    tag_pattern = re.compile(r'#@range_(begin|end)\(([^)]+)\)')
    js_tags = set()
    for match in tag_pattern.finditer(js_content):
        js_tags.add(match.group(2))

    # TSに存在するタグを確認
    ts_tags = set()
    for match in tag_pattern.finditer(ts_content):
        ts_tags.add(match.group(2))

    # 差分
    missing = js_tags - ts_tags
    return missing

test_dir = Path('/home/emile/projects/functionaljs/test')
js_files = sorted(test_dir.glob('*.spec.js'))

all_missing = {}
for js_file in js_files:
    ts_file = js_file.with_suffix('.ts')
    if ts_file.exists():
        missing = find_missing_tags(str(js_file), str(ts_file))
        if missing:
            all_missing[js_file.name] = missing

print("=== 不足しているタグ ===\n")
total = 0
for filename, tags in all_missing.items():
    print(f"{filename.replace('.spec.js', '.spec.ts')}:")
    for tag in sorted(tags):
        print(f"  - {tag}")
        total += 1
    print()

print(f"合計: {total}個のタグペア")
