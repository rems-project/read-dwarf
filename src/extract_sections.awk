BEGINFILE {
    print "% Extracted ott sections", SECS, "from", FILENAME

    split(SECS,sections,",")

    for(i in sections) {
        take[sections[i]] = 0
    }
    keep = 0

}

match($0, /^%%%%% (.*) %%%%%$/, matches)  {
    sec = matches[1]
    if (sec in take) {
        keep = 1
        take[sec] = 1
    }
    else {
        keep = 0
    }
}

{if (keep) print}

ENDFILE{
    err = 0
    for (sec in take){
        if (!take[sec]){
            print sec, "not taken"
            err = 1
        }
    }
    exit err
}
