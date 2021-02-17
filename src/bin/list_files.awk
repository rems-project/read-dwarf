BEGIN{
    RS=" ";
    FS="."
    ORS="\n";
}

/(.*)ml/ {
    split($1,arr,"/")
    filename = arr[length(arr)]
    print toupper(substr(filename,1,1)) substr(filename,2)
}
