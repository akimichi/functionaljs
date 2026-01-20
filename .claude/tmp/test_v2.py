#!/usr/bin/env python3
import sys
sys.path.insert(0, '/home/emile/projects/functionaljs/.claude/tmp')
from copy_review_tags_v2 import process_file_pair

js_path = '/home/emile/projects/functionaljs/test/chap01.spec.js'
ts_path = '/home/emile/projects/functionaljs/test/chap01.spec.ts'

# バックアップ
with open(ts_path, 'r', encoding='utf-8') as f:
    original = f.read()

try:
    result = process_file_pair(js_path, ts_path, verbose=True)

    print("\n挿入されたタグの位置を確認:")
    with open(ts_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        for i, line in enumerate(lines, 1):
            if '#@range_' in line:
                # 前後の行も表示
                context_start = max(0, i - 2)
                context_end = min(len(lines), i + 2)
                print(f"\n行 {i}: {line.rstrip()}")
                print(f"  前の行: {lines[i-2].rstrip() if i > 1 else 'N/A'}")
                print(f"  次の行: {lines[i].rstrip() if i < len(lines) else 'N/A'}")

except Exception as e:
    import traceback
    traceback.print_exc()
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.write(original)
