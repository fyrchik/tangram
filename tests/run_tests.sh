#!/bin/bash

exit_code=0

for file in *.m; do
    file="${file%.*}"
    mmc --rebuild --use-subdirs $file 2>/dev/null
    if [ $? -ne 0 ]; then
        echo $file -- build failed
        exit_code=1
    else
        output=`bash -c ./$file`
        if [ "$output" != "ok" ]; then
            echo $file -- fail
            echo ./$output
            exit_code=1
       else
            echo $file -- ok
        fi
        rm $file
    fi
done

exit $exit_code
