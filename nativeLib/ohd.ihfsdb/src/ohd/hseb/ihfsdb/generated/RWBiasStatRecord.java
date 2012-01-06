// filename: RWBiasStatRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RWBiasStat table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RWBiasStatRecord extends DbRecord
{
    private String office_id;

    private float min_gr_value_bias;

    private int npair_bias_select;

    private int npair_svar_update;

    private int std_cut;

    private int lag_cut;

    private int init_span;

    private int bias_qc_opt;

    private int num_span;

    private float mem_span1;

    private float mem_span2;

    private float mem_span3;

    private float mem_span4;

    private float mem_span5;

    private float mem_span6;

    private float mem_span7;

    private float mem_span8;

    private float mem_span9;

    private float mem_span10;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RWBiasStatRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RWBiasStatRecord(RWBiasStatRecord origRecord)
    {
        setOffice_id(origRecord.getOffice_id());
        setMin_gr_value_bias(origRecord.getMin_gr_value_bias());
        setNpair_bias_select(origRecord.getNpair_bias_select());
        setNpair_svar_update(origRecord.getNpair_svar_update());
        setStd_cut(origRecord.getStd_cut());
        setLag_cut(origRecord.getLag_cut());
        setInit_span(origRecord.getInit_span());
        setBias_qc_opt(origRecord.getBias_qc_opt());
        setNum_span(origRecord.getNum_span());
        setMem_span1(origRecord.getMem_span1());
        setMem_span2(origRecord.getMem_span2());
        setMem_span3(origRecord.getMem_span3());
        setMem_span4(origRecord.getMem_span4());
        setMem_span5(origRecord.getMem_span5());
        setMem_span6(origRecord.getMem_span6());
        setMem_span7(origRecord.getMem_span7());
        setMem_span8(origRecord.getMem_span8());
        setMem_span9(origRecord.getMem_span9());
        setMem_span10(origRecord.getMem_span10());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RWBiasStat record

    //-----------------------------------------------------------------
    public String getOffice_id()
    {
        return office_id;
    }

    public void setOffice_id(String office_id)
    {
        this.office_id = office_id ;
    }

    public float getMin_gr_value_bias()
    {
        return min_gr_value_bias;
    }

    public void setMin_gr_value_bias(float min_gr_value_bias)
    {
        this.min_gr_value_bias = min_gr_value_bias ;
    }

    public int getNpair_bias_select()
    {
        return npair_bias_select;
    }

    public void setNpair_bias_select(int npair_bias_select)
    {
        this.npair_bias_select = npair_bias_select ;
    }

    public int getNpair_svar_update()
    {
        return npair_svar_update;
    }

    public void setNpair_svar_update(int npair_svar_update)
    {
        this.npair_svar_update = npair_svar_update ;
    }

    public int getStd_cut()
    {
        return std_cut;
    }

    public void setStd_cut(int std_cut)
    {
        this.std_cut = std_cut ;
    }

    public int getLag_cut()
    {
        return lag_cut;
    }

    public void setLag_cut(int lag_cut)
    {
        this.lag_cut = lag_cut ;
    }

    public int getInit_span()
    {
        return init_span;
    }

    public void setInit_span(int init_span)
    {
        this.init_span = init_span ;
    }

    public int getBias_qc_opt()
    {
        return bias_qc_opt;
    }

    public void setBias_qc_opt(int bias_qc_opt)
    {
        this.bias_qc_opt = bias_qc_opt ;
    }

    public int getNum_span()
    {
        return num_span;
    }

    public void setNum_span(int num_span)
    {
        this.num_span = num_span ;
    }

    public float getMem_span1()
    {
        return mem_span1;
    }

    public void setMem_span1(float mem_span1)
    {
        this.mem_span1 = mem_span1 ;
    }

    public float getMem_span2()
    {
        return mem_span2;
    }

    public void setMem_span2(float mem_span2)
    {
        this.mem_span2 = mem_span2 ;
    }

    public float getMem_span3()
    {
        return mem_span3;
    }

    public void setMem_span3(float mem_span3)
    {
        this.mem_span3 = mem_span3 ;
    }

    public float getMem_span4()
    {
        return mem_span4;
    }

    public void setMem_span4(float mem_span4)
    {
        this.mem_span4 = mem_span4 ;
    }

    public float getMem_span5()
    {
        return mem_span5;
    }

    public void setMem_span5(float mem_span5)
    {
        this.mem_span5 = mem_span5 ;
    }

    public float getMem_span6()
    {
        return mem_span6;
    }

    public void setMem_span6(float mem_span6)
    {
        this.mem_span6 = mem_span6 ;
    }

    public float getMem_span7()
    {
        return mem_span7;
    }

    public void setMem_span7(float mem_span7)
    {
        this.mem_span7 = mem_span7 ;
    }

    public float getMem_span8()
    {
        return mem_span8;
    }

    public void setMem_span8(float mem_span8)
    {
        this.mem_span8 = mem_span8 ;
    }

    public float getMem_span9()
    {
        return mem_span9;
    }

    public void setMem_span9(float mem_span9)
    {
        this.mem_span9 = mem_span9 ;
    }

    public float getMem_span10()
    {
        return mem_span10;
    }

    public void setMem_span10(float mem_span10)
    {
        this.mem_span10 = mem_span10 ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE office_id = '" + office_id + "'" 
                ;
        return outString;
    } // end toString()
//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getOffice_id() + " " +
                getMin_gr_value_bias() + " " +
                getNpair_bias_select() + " " +
                getNpair_svar_update() + " " +
                getStd_cut() + " " +
                getLag_cut() + " " +
                getInit_span() + " " +
                getBias_qc_opt() + " " +
                getNum_span() + " " +
                getMem_span1() + " " +
                getMem_span2() + " " +
                getMem_span3() + " " +
                getMem_span4() + " " +
                getMem_span5() + " " +
                getMem_span6() + " " +
                getMem_span7() + " " +
                getMem_span8() + " " +
                getMem_span9() + " " +
                getMem_span10() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RWBiasStatRecord class

