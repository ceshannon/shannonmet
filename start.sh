#!/bin/sh
erl -pa /Users/ceshannon/code/shannonmet/ebin -pa /Users/ceshannon/code/shannonmet/deps/*/ebin -s shannonmet -eval "io:format(\"Point your browser at http://localhost:8080~n\")."
