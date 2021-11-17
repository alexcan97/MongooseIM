#!/usr/bin/env bash

# Needs DB_NAME and SA_PASSWORD env variables

SQL="/opt/mssql-tools/bin/sqlcmd"
function do_query {
    set -e
    $SQL -b -S localhost -U sa -P "$SA_PASSWORD" -Q "$1"
}

wait_for()
{
    start_ts=$(date +%s)
    while :
    do
        "$@"
        result=$?
        if [[ $result -eq 0 ]]; then
            end_ts=$(date +%s)
            echo "success after $((end_ts - start_ts)) seconds"
            break
        fi
        sleep 1
    done
    return $result
}

set -e
wait_for do_query "SELECT 1"
do_query "CREATE DATABASE $DB_NAME"
do_query "ALTER DATABASE $DB_NAME SET READ_COMMITTED_SNAPSHOT ON"
do_query "ALTER DATABASE $DB_NAME SET READ_COMMITTED_SNAPSHOT ON"

$SQL -S localhost -U sa -P "$SA_PASSWORD" -d "$DB_NAME" -i mongoose.sql

if [ -z "$SCHEMA_READY_PORT" ]; then
    echo "SCHEMA_READY_PORT not provided"
else
    # Listen on a port to signal for healthcheck that we are ready
    echo "Listening for $SCHEMA_READY_PORT for healthcheck"
    perl -MIO::Socket::INET -ne 'BEGIN{$l=IO::Socket::INET->new(LocalPort => '${SCHEMA_READY_PORT}', Proto=>"tcp", Listen=>5, ReuseAddr=>1); $l=$l->accept}'
fi
