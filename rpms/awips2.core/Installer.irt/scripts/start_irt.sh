#!/bin/sh

IRT_DIR=%{INSTALL_PATH}%/IRT-operational/server/
PID_FILE=$IRT_DIR/tmp.pid

start(){

        cd $IRT_DIR
        python RoutingTableSvc.py &
        routingSvcName=RoutingTableSvc.py
        ps ax | grep "$routingSvcName$" | awk '{print $1;}' > $PID_FILE
}

stop(){
        cd $IRT_DIR
        cat $PID_FILE | while read pid
        do
        kill -9 $pid
        done
        rm $PID_FILE
}

restart(){
        stop
        sleep 3
        start
}

# See how we were called
case $1 in
  start)
        echo "Starting IRT Server..."
        start
        RETVAL=$?
        ;;
  stop)
        echo "Stopping IRT Server..."
        stop
        RETVAL=$?
        ;;
  restart)
        echo "Restarting IRT Server..."
        restart
        RETVAL=$?
        ;;
  *)
        #Print help
        echo "Usage: $0 {start|stop|restart}" 1>&2
        exit 1
        ;;
esac

exit $RETVAL
