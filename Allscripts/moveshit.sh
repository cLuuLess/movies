for dir in `find . -type d`
do
    numfiles=`ls -1 $dir | wc -l`
    if [ $numfiles -gt 90 ]; then
	mv $dir ../Newshit
    fi
done
