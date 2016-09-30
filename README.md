Simple tool for transforming table formats similar to CSV.

Installation
============

    > ./Setup configure
    > ./Setup build
    > ./Setup install

Usage
=====

    Usage: delimtabletransform [OPTION...] files...
        --column-delimiter=DELIMITER      column delimiters (default ['!*!'])
        --row-delimiter=DELIMITER         row delimiters (default ['\n', '\r\n'])
        --substitution=TEXT=TEXT          substitution text on the form <from text>=<to text>
        --encoding=ENCODING               Character encoding of the input files.
        --enclosed-by=TEXT                Character sequence to enclose fields by
        --num-columns=COLUMNS             Number of columns in the input.
        --output-row-delimiter=DELIMITER  Output row delimiter (default '!!!\x1e\n')
