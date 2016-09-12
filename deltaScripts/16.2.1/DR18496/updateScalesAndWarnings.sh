#!/bin/bash
# DR18496 Updates for scales and warning index file

files=`find /awips2/edex/data/utility/cave_static/site/*/bundles/scales -name 'scalesInfo.xml'`   

for f in $files; do
    echo "Moving aside $f - base will be used until site level file is merged"
    mv $f ${f}.orig
done

files=`find /awips2/edex/data/utility/cave_static/site/*/menus/warnings -name 'index.xml'`

for f in $files; do
    grep -q CRsites $f
    if [ $? = 0 ]; then
       $f already has replacement variables
       continue
    fi
    echo Updating $f with new replacement variables
    scp $f ${f}.orig
    grep -v "</menuContributionFile>" $f | grep -v "</include>" > ${f}.tmp
echo -e '        <substitute key="ARsites" value="PAFC, PAFG, PAJK" />
        <substitute key="CRsites"
            value="KABR, KBIS, KCYS, KLOT, KDVN, KBOU, KDMX, KDTX, KDDC, KFGF, KGLD, KGJT, KGRR, KGRB, KGID, KIND, KJKL, KEAX, KARX, KILX, KLMK, KMQT, KMKX, KMPX, KLBF, KAPX, KIWX, KOAX, KPAH, KPUB, KUNR, KRIW, KFSD, KSGF, KLSX, KTOP, KICT" />
        <substitute key="ERsites"
            value="KALY, KLWX, KBGM, KBOX, KBUF, KBTV, KCAR, KCTP, KCHS, KRLX, KILN, KCLE, KCAE, KGSP, KMHX, KOKX, KPHI, KPBZ, KGYX, KRAH, KRNK, KAKQ, KILM" />
        <substitute key="PRsites" value="PHFO, PGUM, NSTU" />
        <substitute key="SRsites"
            value="KABQ, KAMA, KFFC, KEWX, KBMX, KBRO, KCRP, KEPZ, KFWD, KHGX, KHUN, KJAN, KJAX, KKEY, KMRX, KLCH, KLZK, KLUB, KMLB, KMEG, KMFL, KMAF, KMOB, KOHX, KLIX, KOUN, KSJT, KSHV, KTAE, KTBW, KTSA" />
        <substitute key="WRsites"
            value="KBYZ, KBOI, KLKN, KEKA, KFGZ, KGGW, KTFX, KVEF, KLOX, KMFR, KMSO, KPDT, KPSR, KPIH, KPQR, KREV, KSTO, KSLC, KSGX, KMTR, KHNX, KSEW, KOTX, KTWC" />
        <substitute key="ALLsites"
            value="KABQ, KABR, KAKQ, KALY, KAMA, KAPX, KARX, KBGM, KBIS, KBMX, KBOI, KBOU, KBOX, KBRO, KBTV, KBUF, KBYZ, KCAE, KCAR, KCHS, KCLE, KCRP, KCTP, KCYS, KDDC, KDLH, KDMX, KDTX, KDVN, KEAX, KEKA, KEPZ, KEWX, KFFC, KFGF, KFGZ, KFSD, KFWD, KGGW, KGID, KGJT, KGLD, KGRB, KGRR, KGSP, KGYX, KHGX, KHNX, KHUN, KICT, KILM, KILN, KILX, KIND, KIWX, KJAN, KJAX, KJKL, KKEY, KLBF, KLCH, KLIX, KLKN, KLMK, KLOT, KLOX, KLSX, KLUB, KLWX, KLZK, KMAF, KMEG, KMFL, KMFR, KMHX, KMKX, KMLB, KMOB, KMPX, KMQT, KMRX, KMSO, KMTR, KOAX, KOHX, KOKX, KOTX, KOUN, KPAH, KPBZ, KPDT, KPHI, KPIH, KPQR, KPSR, KPUB, KRAH, KREV, KRIW, KRLX, KRNK, KSEW, KSGF, KSGX, KSHV, KSJT, KSLC, KSTO, KTAE, KTBW, KTFX, KTOP, KTSA, KTWC, KUNR, KVEF, PAFC, PAFG, PAJK, PGUM, PHFO, TJSJ, NSTU" />
    </include>
</menuContributionFile>
' >> $f.tmp
mv -f ${f}.tmp $f
chown awips:awips $f; chmod 664 $f
done
