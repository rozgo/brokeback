SCRIPT_PATH=`dirname $0`
curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X POST --data-binary "@$SCRIPT_PATH/sample-receipt-verification.json" http://localhost:9000/r

echo