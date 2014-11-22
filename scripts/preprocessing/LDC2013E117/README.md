`make_splits.sh` makes the splits used in the ACL 2014 paper.

Test is anything in year 2008

Dev is anything in year 2007

Train is everything else

    > grep '::preferred' amr-release-proxy.txt | sed 's/.*\(::id [^ ]*\).*\(::annotator [^ ]*\).*/\1 \2/' | sort | grep '_2008' | wc -l
    2132
    > grep '::preferred' amr-release-proxy.txt | sed 's/.*\(::id [^ ]*\).*\(::annotator [^ ]*\).*/\1 \2/' | sort | grep '_2007' | wc -l
    2132
    > grep '::preferred' amr-release-proxy.txt | sed 's/.*\(::id [^ ]*\).*\(::annotator [^ ]*\).*/\1 \2/' | sort | grep -v '_200[78]' | wc -l
    3955
