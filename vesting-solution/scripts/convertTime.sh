
endPoint=$(python3 -c "refTime=1660498238433;startTime=${1};duration=${2};timeUnit=${3};start=refTime+startTime*timeUnit;print(start + duration*timeUnit)")
echo "Converted Datetime"
python3 -c "import datetime;print(datetime.datetime.utcfromtimestamp(${endPoint}/1000).strftime('%Y-%m-%dT%H:%M:%SZ'))"

echo
echo "Current Datetime"
python3 -c "import datetime;print(datetime.datetime.utcfromtimestamp($(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)/1000).strftime('%Y-%m-%dT%H:%M:%SZ'))"

echo
echo "Difference in Mins"
python3 -c "a=${endPoint}/1000;b=$(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)/1000;c = a - b;print(c/60);print('Locked?', c>=0)"