#!/bin/sh
[ $# -eq 2 ] || { echo "Usage: $0 START END" >&2; exit 2; }
START=$1; END=$2
XLSX="Questions_with_answer.xlsx"
NROWS=5
mkdir -p raw_responses

for id in $(awk "BEGIN{for(i=$START;i<=$END;i++)print i}"); do
  echo "Running id=$id"
  python3 run_chatgpt_single_agent.py "$XLSX" "$NROWS" "$id" || { echo "failed id=$id" >&2; exit 1; }
  sleep 10
done

echo "All done."