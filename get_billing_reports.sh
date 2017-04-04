#!/bin/bash
bucket=$1

if [[ -n "$bucket" ]];then
    dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)
    echo $dir
    mkdir .billings
    aws s3 sync s3://$bucket .billings
    cd .billings && unzip -o "*.zip" && cd ..
else
    echo "Please pass bucket name"
fi
