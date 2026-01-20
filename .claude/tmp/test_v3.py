#!/usr/bin/env python3
import sys
sys.path.insert(0, '/home/emile/projects/functionaljs/.claude/tmp')
from copy_review_tags_v3 import process_file_pair

js_path = '/home/emile/projects/functionaljs/test/chap01.spec.js'
ts_path = '/home/emile/projects/functionaljs/test/chap01.spec.ts'

with open(ts_path, 'r', encoding='utf-8') as f:
    original = f.read()

try:
    result = process_file_pair(js_path, ts_path, verbose=True)

    print("\n挿入されたタグ:")
    with open(ts_path, 'r', encoding='utf-8') as f:
        for i, line in enumerate(f, 1):
            if '#@range_' in line:
                print(f"  {i}: {line.rstrip()}")

except Exception as e:
    import traceback
    traceback.print_exc()
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.write(original)
