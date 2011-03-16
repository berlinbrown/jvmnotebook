#!/bin/sh
# client.sh

http_proxy=""

for i in { 1..70 } ; do 
	echo 'Connecting iter=${i}'     
	curl -s 'http://127.0.0.1:9999/'   
	sleep 2 ;
done
