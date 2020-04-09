BEGIN{
    RS="\n\n";
    ORS="\n\n";
}

{
    if (!($0 ~ "\nmodule")) print;
}
