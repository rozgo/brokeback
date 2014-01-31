#!/bin/bash

SCRIPT_PATH=`dirname $0`

echo "Testing valid receipt data..."
curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X POST --data-binary "@$SCRIPT_PATH/sample-receipt-verification.json" http://localhost:9000/r

echo
echo

echo "Testing invalid receipt data..."
curl -i -H "Accept: application/json" -H "Content-Type: application/json" -X POST --data-binary "@$SCRIPT_PATH/sample-receipt-failure.json" http://localhost:9000/r

echo