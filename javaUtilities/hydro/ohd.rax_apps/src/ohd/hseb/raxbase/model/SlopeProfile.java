package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.SlopeprofileRecord;

public class SlopeProfile
{
    private String lid;
    private String marker;
    private long begdate;
    private long enddate;
    private float distance01;
    private float distance02;
    private float distance03;
    private float distance04;
    private float distance05;
    private float distance06;
    private float distance07;
    private float distance08;
    private float distance09;
    private float distance10;
    private float distance11;
    private float distance12;
    private float distance13;
    private float distance14;
    private float distance15;
    private float distance16;
    private float distance17;
    private float distance18;
    private float distance19;
    private float distance20;
    private float distance21;
    private float distance22;
    private float distance23;
    private float distance24;
    private float distance25;
    private float distance26;
    private float distance27;
    private float distance28;
    private float distance29;
    private float distance30;
    private float elevation01;
    private float elevation02;
    private float elevation03;
    private float elevation04;
    private float elevation05;
    private float elevation06;
    private float elevation07;
    private float elevation08;
    private float elevation09;
    private float elevation10;
    private float elevation11;
    private float elevation12;
    private float elevation13;
    private float elevation14;
    private float elevation15;
    private float elevation16;
    private float elevation17;
    private float elevation18;
    private float elevation19;
    private float elevation20;
    private float elevation21;
    private float elevation22;
    private float elevation23;
    private float elevation24;
    private float elevation25;
    private float elevation26;
    private float elevation27;
    private float elevation28;
    private float elevation29;
    private float elevation30;

    
    public SlopeProfile(){}
    
    public SlopeProfile(SlopeProfile slopeProfile)
    {
        setLid(slopeProfile.getLid());
        setMarker(slopeProfile.getMarker());
        setBegdate(slopeProfile.getBegdate());
        setEnddate(slopeProfile.getEnddate());
        setDistance01(slopeProfile.getDistance01());
        setDistance02(slopeProfile.getDistance02());
        setDistance03(slopeProfile.getDistance03());
        setDistance04(slopeProfile.getDistance04());
        setDistance05(slopeProfile.getDistance05());
        setDistance06(slopeProfile.getDistance06());
        setDistance07(slopeProfile.getDistance07());
        setDistance08(slopeProfile.getDistance08());
        setDistance09(slopeProfile.getDistance09());
        setDistance10(slopeProfile.getDistance10());
        setDistance11(slopeProfile.getDistance11());
        setDistance12(slopeProfile.getDistance12());
        setDistance13(slopeProfile.getDistance13());
        setDistance14(slopeProfile.getDistance14());
        setDistance15(slopeProfile.getDistance15());
        setDistance16(slopeProfile.getDistance16());
        setDistance17(slopeProfile.getDistance17());
        setDistance18(slopeProfile.getDistance18());
        setDistance19(slopeProfile.getDistance19());
        setDistance20(slopeProfile.getDistance20());
        setDistance21(slopeProfile.getDistance21());
        setDistance22(slopeProfile.getDistance22());
        setDistance23(slopeProfile.getDistance23());
        setDistance24(slopeProfile.getDistance24());
        setDistance25(slopeProfile.getDistance25());
        setDistance26(slopeProfile.getDistance26());
        setDistance27(slopeProfile.getDistance27());
        setDistance28(slopeProfile.getDistance28());
        setDistance29(slopeProfile.getDistance29());
        setDistance30(slopeProfile.getDistance30());
        setElevation01(slopeProfile.getElevation01());
        setElevation02(slopeProfile.getElevation02());
        setElevation03(slopeProfile.getElevation03());
        setElevation04(slopeProfile.getElevation04());
        setElevation05(slopeProfile.getElevation05());
        setElevation06(slopeProfile.getElevation06());
        setElevation07(slopeProfile.getElevation07());
        setElevation08(slopeProfile.getElevation08());
        setElevation09(slopeProfile.getElevation09());
        setElevation10(slopeProfile.getElevation10());
        setElevation11(slopeProfile.getElevation11());
        setElevation12(slopeProfile.getElevation12());
        setElevation13(slopeProfile.getElevation13());
        setElevation14(slopeProfile.getElevation14());
        setElevation15(slopeProfile.getElevation15());
        setElevation16(slopeProfile.getElevation16());
        setElevation17(slopeProfile.getElevation17());
        setElevation18(slopeProfile.getElevation18());
        setElevation19(slopeProfile.getElevation19());
        setElevation20(slopeProfile.getElevation20());
        setElevation21(slopeProfile.getElevation21());
        setElevation22(slopeProfile.getElevation22());
        setElevation23(slopeProfile.getElevation23());
        setElevation24(slopeProfile.getElevation24());
        setElevation25(slopeProfile.getElevation25());
        setElevation26(slopeProfile.getElevation26());
        setElevation27(slopeProfile.getElevation27());
        setElevation28(slopeProfile.getElevation28());
        setElevation29(slopeProfile.getElevation29());
        setElevation30(slopeProfile.getElevation30());
    }

    public SlopeProfile( SlopeprofileRecord origRecord )
    {
        setLid(origRecord.getLid());
        setMarker(origRecord.getMarker());
        setBegdate(origRecord.getBegdate());
        setEnddate(origRecord.getEnddate());
        setDistance01(origRecord.getDistance01());
        setDistance02(origRecord.getDistance02());
        setDistance03(origRecord.getDistance03());
        setDistance04(origRecord.getDistance04());
        setDistance05(origRecord.getDistance05());
        setDistance06(origRecord.getDistance06());
        setDistance07(origRecord.getDistance07());
        setDistance08(origRecord.getDistance08());
        setDistance09(origRecord.getDistance09());
        setDistance10(origRecord.getDistance10());
        setDistance11(origRecord.getDistance11());
        setDistance12(origRecord.getDistance12());
        setDistance13(origRecord.getDistance13());
        setDistance14(origRecord.getDistance14());
        setDistance15(origRecord.getDistance15());
        setDistance16(origRecord.getDistance16());
        setDistance17(origRecord.getDistance17());
        setDistance18(origRecord.getDistance18());
        setDistance19(origRecord.getDistance19());
        setDistance20(origRecord.getDistance20());
        setDistance21(origRecord.getDistance21());
        setDistance22(origRecord.getDistance22());
        setDistance23(origRecord.getDistance23());
        setDistance24(origRecord.getDistance24());
        setDistance25(origRecord.getDistance25());
        setDistance26(origRecord.getDistance26());
        setDistance27(origRecord.getDistance27());
        setDistance28(origRecord.getDistance28());
        setDistance29(origRecord.getDistance29());
        setDistance30(origRecord.getDistance30());
        setElevation01(origRecord.getElevation01());
        setElevation02(origRecord.getElevation02());
        setElevation03(origRecord.getElevation03());
        setElevation04(origRecord.getElevation04());
        setElevation05(origRecord.getElevation05());
        setElevation06(origRecord.getElevation06());
        setElevation07(origRecord.getElevation07());
        setElevation08(origRecord.getElevation08());
        setElevation09(origRecord.getElevation09());
        setElevation10(origRecord.getElevation10());
        setElevation11(origRecord.getElevation11());
        setElevation12(origRecord.getElevation12());
        setElevation13(origRecord.getElevation13());
        setElevation14(origRecord.getElevation14());
        setElevation15(origRecord.getElevation15());
        setElevation16(origRecord.getElevation16());
        setElevation17(origRecord.getElevation17());
        setElevation18(origRecord.getElevation18());
        setElevation19(origRecord.getElevation19());
        setElevation20(origRecord.getElevation20());
        setElevation21(origRecord.getElevation21());
        setElevation22(origRecord.getElevation22());
        setElevation23(origRecord.getElevation23());
        setElevation24(origRecord.getElevation24());
        setElevation25(origRecord.getElevation25());
        setElevation26(origRecord.getElevation26());
        setElevation27(origRecord.getElevation27());
        setElevation28(origRecord.getElevation28());
        setElevation29(origRecord.getElevation29());
        setElevation30(origRecord.getElevation30());
    }
    
    public static SlopeProfile getSlopeProfile( SlopeprofileRecord record )
    {
        return ( new SlopeProfile( record ) );
    }
    
    public static SlopeprofileRecord getSlopeProfileRecord( SlopeProfile slopeProfile )
    {
        SlopeprofileRecord slopeProfileRecord = new SlopeprofileRecord();
        
        slopeProfileRecord.setLid(slopeProfile.getLid());
        slopeProfileRecord.setMarker(slopeProfile.getMarker());
        slopeProfileRecord.setBegdate(slopeProfile.getBegdate());
        slopeProfileRecord.setEnddate(slopeProfile.getEnddate());
        slopeProfileRecord.setDistance01(slopeProfile.getDistance01());
        slopeProfileRecord.setDistance02(slopeProfile.getDistance02());
        slopeProfileRecord.setDistance03(slopeProfile.getDistance03());
        slopeProfileRecord.setDistance04(slopeProfile.getDistance04());
        slopeProfileRecord.setDistance05(slopeProfile.getDistance05());
        slopeProfileRecord.setDistance06(slopeProfile.getDistance06());
        slopeProfileRecord.setDistance07(slopeProfile.getDistance07());
        slopeProfileRecord.setDistance08(slopeProfile.getDistance08());
        slopeProfileRecord.setDistance09(slopeProfile.getDistance09());
        slopeProfileRecord.setDistance10(slopeProfile.getDistance10());
        slopeProfileRecord.setDistance11(slopeProfile.getDistance11());
        slopeProfileRecord.setDistance12(slopeProfile.getDistance12());
        slopeProfileRecord.setDistance13(slopeProfile.getDistance13());
        slopeProfileRecord.setDistance14(slopeProfile.getDistance14());
        slopeProfileRecord.setDistance15(slopeProfile.getDistance15());
        slopeProfileRecord.setDistance16(slopeProfile.getDistance16());
        slopeProfileRecord.setDistance17(slopeProfile.getDistance17());
        slopeProfileRecord.setDistance18(slopeProfile.getDistance18());
        slopeProfileRecord.setDistance19(slopeProfile.getDistance19());
        slopeProfileRecord.setDistance20(slopeProfile.getDistance20());
        slopeProfileRecord.setDistance21(slopeProfile.getDistance21());
        slopeProfileRecord.setDistance22(slopeProfile.getDistance22());
        slopeProfileRecord.setDistance23(slopeProfile.getDistance23());
        slopeProfileRecord.setDistance24(slopeProfile.getDistance24());
        slopeProfileRecord.setDistance25(slopeProfile.getDistance25());
        slopeProfileRecord.setDistance26(slopeProfile.getDistance26());
        slopeProfileRecord.setDistance27(slopeProfile.getDistance27());
        slopeProfileRecord.setDistance28(slopeProfile.getDistance28());
        slopeProfileRecord.setDistance29(slopeProfile.getDistance29());
        slopeProfileRecord.setDistance30(slopeProfile.getDistance30());
        slopeProfileRecord.setElevation01(slopeProfile.getElevation01());
        slopeProfileRecord.setElevation02(slopeProfile.getElevation02());
        slopeProfileRecord.setElevation03(slopeProfile.getElevation03());
        slopeProfileRecord.setElevation04(slopeProfile.getElevation04());
        slopeProfileRecord.setElevation05(slopeProfile.getElevation05());
        slopeProfileRecord.setElevation06(slopeProfile.getElevation06());
        slopeProfileRecord.setElevation07(slopeProfile.getElevation07());
        slopeProfileRecord.setElevation08(slopeProfile.getElevation08());
        slopeProfileRecord.setElevation09(slopeProfile.getElevation09());
        slopeProfileRecord.setElevation10(slopeProfile.getElevation10());
        slopeProfileRecord.setElevation11(slopeProfile.getElevation11());
        slopeProfileRecord.setElevation12(slopeProfile.getElevation12());
        slopeProfileRecord.setElevation13(slopeProfile.getElevation13());
        slopeProfileRecord.setElevation14(slopeProfile.getElevation14());
        slopeProfileRecord.setElevation15(slopeProfile.getElevation15());
        slopeProfileRecord.setElevation16(slopeProfile.getElevation16());
        slopeProfileRecord.setElevation17(slopeProfile.getElevation17());
        slopeProfileRecord.setElevation18(slopeProfile.getElevation18());
        slopeProfileRecord.setElevation19(slopeProfile.getElevation19());
        slopeProfileRecord.setElevation20(slopeProfile.getElevation20());
        slopeProfileRecord.setElevation21(slopeProfile.getElevation21());
        slopeProfileRecord.setElevation22(slopeProfile.getElevation22());
        slopeProfileRecord.setElevation23(slopeProfile.getElevation23());
        slopeProfileRecord.setElevation24(slopeProfile.getElevation24());
        slopeProfileRecord.setElevation25(slopeProfile.getElevation25());
        slopeProfileRecord.setElevation26(slopeProfile.getElevation26());
        slopeProfileRecord.setElevation27(slopeProfile.getElevation27());
        slopeProfileRecord.setElevation28(slopeProfile.getElevation28());
        slopeProfileRecord.setElevation29(slopeProfile.getElevation29());
        slopeProfileRecord.setElevation30(slopeProfile.getElevation30());

        return slopeProfileRecord;
    }
    
    public String keyString()
    {
        return "Lid = " + lid +
               " | Marker = " + marker + 
               " | BeginDate = " + begdate;
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Slopeprofile record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getMarker()
    {
        return marker;
    }

    public void setMarker(String marker)
    {
        this.marker = marker ;
    }

    public long getBegdate()
    {
        return begdate;
    }

    public void setBegdate(long begdate)
    {
        this.begdate = begdate ;
    }

    public long getEnddate()
    {
        return enddate;
    }

    public void setEnddate(long enddate)
    {
        this.enddate = enddate ;
    }

    public float getDistance01()
    {
        return distance01;
    }

    public void setDistance01(float distance01)
    {
        this.distance01 = distance01 ;
    }

    public float getDistance02()
    {
        return distance02;
    }

    public void setDistance02(float distance02)
    {
        this.distance02 = distance02 ;
    }

    public float getDistance03()
    {
        return distance03;
    }

    public void setDistance03(float distance03)
    {
        this.distance03 = distance03 ;
    }

    public float getDistance04()
    {
        return distance04;
    }

    public void setDistance04(float distance04)
    {
        this.distance04 = distance04 ;
    }

    public float getDistance05()
    {
        return distance05;
    }

    public void setDistance05(float distance05)
    {
        this.distance05 = distance05 ;
    }

    public float getDistance06()
    {
        return distance06;
    }

    public void setDistance06(float distance06)
    {
        this.distance06 = distance06 ;
    }

    public float getDistance07()
    {
        return distance07;
    }

    public void setDistance07(float distance07)
    {
        this.distance07 = distance07 ;
    }

    public float getDistance08()
    {
        return distance08;
    }

    public void setDistance08(float distance08)
    {
        this.distance08 = distance08 ;
    }

    public float getDistance09()
    {
        return distance09;
    }

    public void setDistance09(float distance09)
    {
        this.distance09 = distance09 ;
    }

    public float getDistance10()
    {
        return distance10;
    }

    public void setDistance10(float distance10)
    {
        this.distance10 = distance10 ;
    }

    public float getDistance11()
    {
        return distance11;
    }

    public void setDistance11(float distance11)
    {
        this.distance11 = distance11 ;
    }

    public float getDistance12()
    {
        return distance12;
    }

    public void setDistance12(float distance12)
    {
        this.distance12 = distance12 ;
    }

    public float getDistance13()
    {
        return distance13;
    }

    public void setDistance13(float distance13)
    {
        this.distance13 = distance13 ;
    }

    public float getDistance14()
    {
        return distance14;
    }

    public void setDistance14(float distance14)
    {
        this.distance14 = distance14 ;
    }

    public float getDistance15()
    {
        return distance15;
    }

    public void setDistance15(float distance15)
    {
        this.distance15 = distance15 ;
    }

    public float getDistance16()
    {
        return distance16;
    }

    public void setDistance16(float distance16)
    {
        this.distance16 = distance16 ;
    }

    public float getDistance17()
    {
        return distance17;
    }

    public void setDistance17(float distance17)
    {
        this.distance17 = distance17 ;
    }

    public float getDistance18()
    {
        return distance18;
    }

    public void setDistance18(float distance18)
    {
        this.distance18 = distance18 ;
    }

    public float getDistance19()
    {
        return distance19;
    }

    public void setDistance19(float distance19)
    {
        this.distance19 = distance19 ;
    }

    public float getDistance20()
    {
        return distance20;
    }

    public void setDistance20(float distance20)
    {
        this.distance20 = distance20 ;
    }

    public float getDistance21()
    {
        return distance21;
    }

    public void setDistance21(float distance21)
    {
        this.distance21 = distance21 ;
    }

    public float getDistance22()
    {
        return distance22;
    }

    public void setDistance22(float distance22)
    {
        this.distance22 = distance22 ;
    }

    public float getDistance23()
    {
        return distance23;
    }

    public void setDistance23(float distance23)
    {
        this.distance23 = distance23 ;
    }

    public float getDistance24()
    {
        return distance24;
    }

    public void setDistance24(float distance24)
    {
        this.distance24 = distance24 ;
    }

    public float getDistance25()
    {
        return distance25;
    }

    public void setDistance25(float distance25)
    {
        this.distance25 = distance25 ;
    }

    public float getDistance26()
    {
        return distance26;
    }

    public void setDistance26(float distance26)
    {
        this.distance26 = distance26 ;
    }

    public float getDistance27()
    {
        return distance27;
    }

    public void setDistance27(float distance27)
    {
        this.distance27 = distance27 ;
    }

    public float getDistance28()
    {
        return distance28;
    }

    public void setDistance28(float distance28)
    {
        this.distance28 = distance28 ;
    }

    public float getDistance29()
    {
        return distance29;
    }

    public void setDistance29(float distance29)
    {
        this.distance29 = distance29 ;
    }

    public float getDistance30()
    {
        return distance30;
    }

    public void setDistance30(float distance30)
    {
        this.distance30 = distance30 ;
    }

    public float getElevation01()
    {
        return elevation01;
    }

    public void setElevation01(float elevation01)
    {
        this.elevation01 = elevation01 ;
    }

    public float getElevation02()
    {
        return elevation02;
    }

    public void setElevation02(float elevation02)
    {
        this.elevation02 = elevation02 ;
    }

    public float getElevation03()
    {
        return elevation03;
    }

    public void setElevation03(float elevation03)
    {
        this.elevation03 = elevation03 ;
    }

    public float getElevation04()
    {
        return elevation04;
    }

    public void setElevation04(float elevation04)
    {
        this.elevation04 = elevation04 ;
    }

    public float getElevation05()
    {
        return elevation05;
    }

    public void setElevation05(float elevation05)
    {
        this.elevation05 = elevation05 ;
    }

    public float getElevation06()
    {
        return elevation06;
    }

    public void setElevation06(float elevation06)
    {
        this.elevation06 = elevation06 ;
    }

    public float getElevation07()
    {
        return elevation07;
    }

    public void setElevation07(float elevation07)
    {
        this.elevation07 = elevation07 ;
    }

    public float getElevation08()
    {
        return elevation08;
    }

    public void setElevation08(float elevation08)
    {
        this.elevation08 = elevation08 ;
    }

    public float getElevation09()
    {
        return elevation09;
    }

    public void setElevation09(float elevation09)
    {
        this.elevation09 = elevation09 ;
    }

    public float getElevation10()
    {
        return elevation10;
    }

    public void setElevation10(float elevation10)
    {
        this.elevation10 = elevation10 ;
    }

    public float getElevation11()
    {
        return elevation11;
    }

    public void setElevation11(float elevation11)
    {
        this.elevation11 = elevation11 ;
    }

    public float getElevation12()
    {
        return elevation12;
    }

    public void setElevation12(float elevation12)
    {
        this.elevation12 = elevation12 ;
    }

    public float getElevation13()
    {
        return elevation13;
    }

    public void setElevation13(float elevation13)
    {
        this.elevation13 = elevation13 ;
    }

    public float getElevation14()
    {
        return elevation14;
    }

    public void setElevation14(float elevation14)
    {
        this.elevation14 = elevation14 ;
    }

    public float getElevation15()
    {
        return elevation15;
    }

    public void setElevation15(float elevation15)
    {
        this.elevation15 = elevation15 ;
    }

    public float getElevation16()
    {
        return elevation16;
    }

    public void setElevation16(float elevation16)
    {
        this.elevation16 = elevation16 ;
    }

    public float getElevation17()
    {
        return elevation17;
    }

    public void setElevation17(float elevation17)
    {
        this.elevation17 = elevation17 ;
    }

    public float getElevation18()
    {
        return elevation18;
    }

    public void setElevation18(float elevation18)
    {
        this.elevation18 = elevation18 ;
    }

    public float getElevation19()
    {
        return elevation19;
    }

    public void setElevation19(float elevation19)
    {
        this.elevation19 = elevation19 ;
    }

    public float getElevation20()
    {
        return elevation20;
    }

    public void setElevation20(float elevation20)
    {
        this.elevation20 = elevation20 ;
    }

    public float getElevation21()
    {
        return elevation21;
    }

    public void setElevation21(float elevation21)
    {
        this.elevation21 = elevation21 ;
    }

    public float getElevation22()
    {
        return elevation22;
    }

    public void setElevation22(float elevation22)
    {
        this.elevation22 = elevation22 ;
    }

    public float getElevation23()
    {
        return elevation23;
    }

    public void setElevation23(float elevation23)
    {
        this.elevation23 = elevation23 ;
    }

    public float getElevation24()
    {
        return elevation24;
    }

    public void setElevation24(float elevation24)
    {
        this.elevation24 = elevation24 ;
    }

    public float getElevation25()
    {
        return elevation25;
    }

    public void setElevation25(float elevation25)
    {
        this.elevation25 = elevation25 ;
    }

    public float getElevation26()
    {
        return elevation26;
    }

    public void setElevation26(float elevation26)
    {
        this.elevation26 = elevation26 ;
    }

    public float getElevation27()
    {
        return elevation27;
    }

    public void setElevation27(float elevation27)
    {
        this.elevation27 = elevation27 ;
    }

    public float getElevation28()
    {
        return elevation28;
    }

    public void setElevation28(float elevation28)
    {
        this.elevation28 = elevation28 ;
    }

    public float getElevation29()
    {
        return elevation29;
    }

    public void setElevation29(float elevation29)
    {
        this.elevation29 = elevation29 ;
    }

    public float getElevation30()
    {
        return elevation30;
    }

    public void setElevation30(float elevation30)
    {
        this.elevation30 = elevation30 ;
    }
}
