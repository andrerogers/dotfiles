#!/bin/bash

USER_GROUP_ID=`id -u $USER`

help() {
	echo "Emacs Controller"
	echo "----------------"
	echo -ne "create\t\tcreate server\n"
	echo -ne "list\t\tview active servers\n"
	echo -ne "kill\t\tkill an active server\n"
	echo -ne "kill-all\tkill all active servers"
}

create_server() {
	echo "INFO: Creating '$1' server!"
	emacs --daemon=$1
}

start_session() {
	echo "INFO: Attempting to create session for '$1' server!"
	emacsclient -c -s $1 -n
	echo "INFO: Created session for '$1' server!"
}

list_servers() {
	validate

	echo "INFO: Listing servers..."

	for SERVER in $SERVERS
	do
		echo -ne "$SERVER\n"
	done
}

kill_server() {
	echo "INFO: Killing $1 server..."
	emacsclient -s $1 -e '(kill-emacs)'
	echo "INFO: Killed $SERVER server..."
}

kill_servers() {
	validate

	echo "INFO: Killing servers..."

	for SERVER in $SERVERS
	do
		kill_server $SERVER
	done
}

validate() {
	SERVERS=`ls /var/run/user/$USER_GROUP_ID/emacs/`

	if [[ ${#SERVERS[0]} -eq " " ]]; then
		echo "INFO: No active servers!"
		exit
	fi

	if [[ ${#SERVERS[@]} -eq 0 ]]; then
		echo "INFO: No active servers!"
		exit
	fi
}

case $1 in
	create)
		if [[ -z "$2" ]]; then
			echo "ERROR: pass a server name"
			exit
		fi

		create_server $2
		;;
	session)
		if [[ -z "$2" ]]; then
			echo "ERROR: pass a valid server name"
			exit
		fi

		start_session $2
		;;
	kill)
		if [[ -z "$2" ]]; then
			echo "ERROR: pass a valid server name"
			exit
		fi

		kill_server $2
		;;
	kill-all)
		kill_servers
		;;
	list)
		list_servers
		;;
	help)
		help
		;;
	*)
		echo "ERROR: Unknown Command, pass help to see args"
		;;
esac

