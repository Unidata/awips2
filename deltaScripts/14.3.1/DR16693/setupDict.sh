#!/bin/bash
#
# Clean up platform and users dictionaries.
# 09/10/2014 lshi
#
#platform dictionary(lx, px): /awips2/cave/etc/spelldict
#user EDEX dictionary(dx):  /awips2/edex/data/utility/cave_static/user/USER/seplldict
#user CAVE dictionary(lx/px/dx): /home/USER/caveData/etc/user/USER/spelldict

#dx (one of dx): 
#remove all users' CAVE dictionary
#cleanup all users' EDEX dictionary
#
#all others:
#clean up platform dictionary
#

user=$(whoami)
host=$(hostname)

edex_user_dir=/awips2/edex/data/utility/cave_static/user/
cave_etc=/awips2/cave/etc
run_type=0
FNAME="spelldict"

clean () {
    lines=`cat $1 |wc -l`
    size=`cat $1 |wc -c`
    MSG="$1, size=$size, #line=$lines:"
    LASTWD=$(grep 'zymurgy' $1)
    if [ $size -eq 1290760 ]
    then
        remove $1
#    elif [ $lines -gt 135553 ]  
#    then
#        [ $run_type == 1 ] && (cp $1 "$1.bak"; 
#            sed -n "135554,${lines}p" "$1.bak" > $1)
#        let "newlines=${lines}-135553"
#        echo $MSG modified, \#line=$(( lines-135553 ))
    elif [ "$LASTWD" ]
    then
        line=$(sed -n "/^$LASTWD/=" $1)
#        echo line=$line
        [ $run_type == 1 ] && (cp -p $1 "$1.bak"; sed "1, /^$LASTWD/d" "$1.bak" > $1)
        echo $MSG "modified, #line=$(( lines-line ))"
    else
        echo $MSG unchanged
    fi
}

remove () {
    lines=`cat $1 |wc -l`
    size=`cat $1 |wc -c`
    if [ $run_type == 1 ]
    then
        cp -p $1 "$1.bak"
        [[ $1 == ${cave_etc}* ]] && cat /dev/null > $1 || rm -f $1
    fi
        
    action=$([[ $1 == ${cave_etc}* ]] && echo emptied || echo removed )
    echo "$1, size=$size, #line=$lines: $action"
}

usage () {
    echo "Option: -dryrun: dry run; -run: do it"
    exit 0
}


[ $# = 0 ] && usage

[ $1 == -run ] && run_type=1
[ $1 == -dryrun ] && run_type=2
[ $run_type == 0 ] && usage
echo "run_type=$run_type"

wstype=xxx
[ $# == 2 ] && wstype=$2

if [ -d $edex_user_dir ] && [ $wstype != -lx ]
then
    echo "Clean up users' dictionaries ..."
    if [ $user != root ] 
    then
        echo "You must run this script as the user 'root'." 
        exit 1 
    fi
    for d in $(ls -d /home/*);
    do
        f=$d/caveData/etc/user/$(basename $d)/$FNAME
        [ -f $f ] && remove $f
    done  

    for f in `find $edex_user_dir -maxdepth 2 -name $FNAME`;
    do
        clean $f
    done  
fi

if [ -d $cave_etc ] && [ $wstype != -dx ]
then
    f=$cave_etc/$FNAME
    echo "Clean up the platform dictionary ${f} ..."
    if [ $user != awips ] && [ $user != root ]
    then
        echo "You must run this script as the user 'awips' or 'root'." 
        exit 1
    fi
    if [ -f $f ] 
    then
        clean $f 
    else
        cat /dev/null > $f
        chown awips $f
        chgrp fxalpha $f
        chmod 644 $f
        echo $f: created, size=0
    fi
fi

if [ ! -d $edex_user_dir ] && [ ! -d $cave_etc ]
then
    echo "Please run this script on a 'dx', 'lx', px or 'xt' workstation. "
    exit 1
fi

exit 0

