#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Fetch a key from the AWS authentication config file
# Arguments:
#   $1  - Environment name: e.g prod, stage or default
#   $2 - Key name, e.g aws_access_key_id
# Returns:
#   String
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
get_awsconfig_entry() {
    local polyenv="$1"
    local key="$2"
    local creds="$HOME/.aws/credentials"

    if [[ ! -e "$creds" ]]; then
        echo "No file at $creds"
        return 3
    fi

    cat << EOF | awk -f - "$creds"
        (\$2 != "=") { gsub(/[\[\]]/, "", \$0); cenv=\$0; }
        (\$2 = "=") { tbl[cenv][\$1]=\$3; }
        END {
          if ("$polyenv" in tbl)
             if ("$key" in tbl["$polyenv"]) print tbl["$polyenv"]["$key"]
             else print "key: ", "$key", "not found" > "/dev/stderr"
          else
            print "Environment not found" > "/dev/stderr"
       }
EOF
}

#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Set the current AWS enviroment variables to those of a
# particular section of the AWS authentication config file.
# Arguments:
#   $1  - Environment name: e.g prod, stage or default
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
poly_setenv() {
    if [[ "$1" = "stage" || "$1" = "prod" ]]; then
        export AWS_ACCESS_KEY_ID="$(get_awsconfig_entry $1 aws_access_key_id)"
        export AWS_SECRET_ACCESS_KEY="$(get_awsconfig_entry $1 aws_secret_access_key)"
    else
        echo "bad environment name"
    fi
}
poly_setenv stage

#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Connect to the polyswarm bastion host
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
poly_connectdocker() {
    if (( $# == 0 )); then # Staging, default
        local POLY_WORK=stage
    else
        local POLY_WORK=$1
    fi
    ssh -NL localhost:2374:/var/run/docker.sock poly@b."$POLY_WORK".polyswarm.network &
    export DOCKER_HOST=localhost:2374
    export DOCKER_SSH_PID=$!
}

#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Disconnect from said docker instance
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
poly_disconnectdocker() {
    kill $DOCKER_SSH_PID
    unset DOCKER_SSH_PID
}

#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Determine the polyswarm environment our AWS keys belong to.
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
poly_checkenv() {
    if [[ "$AWS_ACCESS_KEY_ID" = "$(get_awsconfig_entry prod aws_access_key_id)" ]]; then
        echo prod
    elif [[ "$AWS_ACCESS_KEY_ID" = "$(get_awsconfig_entry stage aws_access_key_id)" ]]; then
        echo stage
    else
        echo "AWS_ACCESS_KEY_ID not set correctly"
    fi
}

#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
# Dump polyswarm-related environment variables
#╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
poly_reviewenv() {
    local pfix
    for pfix in "\$TF_CLI_ARGS_"{plan,apply,init}; do
        echo "$pfix: $(eval echo $pfix)"
    done

    echo "\$AWS_PROFILE: $AWS_PROFILE"
    echo "\$STATE_BUCKET: $STATE_BUCKET"
    echo "\$WORKSPACE: $WORKSPACE"
    echo "\$AWS_ACCESS_KEY_ID: $AWS_ACCESS_KEY_ID"
    echo "\$AWS_SECRET_ACCESS_KEY: $AWS_SECRET_ACCESS_KEY"
    echo "AWS keys are from $(poly_checkenv)"
}
