#!/usr/bin/env python3
"""
1つのファイルでテスト
"""
import sys
sys.path.insert(0, '/home/emile/projects/functionaljs/.claude/tmp')
from copy_review_tags import process_file_pair

# chap01だけテスト
js_path = '/home/emile/projects/functionaljs/test/chap01.spec.js'
ts_path = '/home/emile/projects/functionaljs/test/chap01.spec.ts'

# バックアップ
with open(ts_path, 'r', encoding='utf-8') as f:
    original = f.read()

try:
    process_file_pair(js_path, ts_path)
    print("\n変更後のファイル（最初の100行）:")
    with open(ts_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()[:100]
        for i, line in enumerate(lines, 1):
            if '#@range_' in line:
                print(f">>> {i}: {line.rstrip()}")
except Exception as e:
    print(f"エラー: {e}")
    # ロールバック
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.write(original)
    raise
