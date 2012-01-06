package ohd.hseb.bias_trans;

public class MemorySpanGroup
{
    private float _minGrValueBias;
    private int _npairBiasSelect;
    private int _npairSvarUpdate;
    private int _stdCut;
    private int _lagCut;
    private int _initSpan;
    private int _biasQcOpt;
    private int _numSpan;
    private float _memSpan1;
    private float _memSpan2;
    private float _memSpan3;
    private float _memSpan4;
    private float _memSpan5;
    private float _memSpan6;
    private float _memSpan7;
    private float _memSpan8;
    private float _memSpan9;
    private float _memSpan10;
    
    public MemorySpanGroup ( )
    {
    }
    
    public void setMinGrValueBias(float minGrValueBias)
    {
        this._minGrValueBias = minGrValueBias;
    }
    public float getMinGrValueBias()
    {
        return _minGrValueBias;
    }

    public void setNpairBiasSelect(int npairBiasSelect)
    {
        this._npairBiasSelect = npairBiasSelect;
    }

    public int getNpairBiasSelect()
    {
        return _npairBiasSelect;
    }

    public void setNpairSvarUpdate(int npairSvarUpdate)
    {
        this._npairSvarUpdate = npairSvarUpdate;
    }

    public int getNpairSvarUpdate()
    {
        return _npairSvarUpdate;
    }

    public void setStdCut(int stdCut)
    {
        this._stdCut = stdCut;
    }

    public int getStdCut()
    {
        return _stdCut;
    }

    public void setLagCut(int lagCut)
    {
        this._lagCut = lagCut;
    }

    public int getLagCut()
    {
        return _lagCut;
    }

    public void setInitSpan(int initSpan)
    {
        this._initSpan = initSpan;
    }

    public int getInitSpan()
    {
        return _initSpan;
    }

    public void setBiasQcOpt(int biasQcOpt)
    {
        this._biasQcOpt = biasQcOpt;
    }

    public int getBiasQcOpt()
    {
        return _biasQcOpt;
    }

    public void setNumSpan(int numSpan)
    {
        this._numSpan = numSpan;
    }

    public int getNumSpan()
    {
        return _numSpan;
    }

    public void setMemSpan1(float memSpan1)
    {
        this._memSpan1 = memSpan1;
    }

    public float getMemSpan1()
    {
        return _memSpan1;
    }

    public void setMemSpan2(float memSpan2)
    {
        this._memSpan2 = memSpan2;
    }

    public float getMemSpan2()
    {
        return _memSpan2;
    }

    public void setMemSpan3(float memSpan3)
    {
        this._memSpan3 = memSpan3;
    }

    public float getMemSpan3()
    {
        return _memSpan3;
    }

    public void setMemSpan4(float memSpan4)
    {
        this._memSpan4 = memSpan4;
    }

    public float getMemSpan4()
    {
        return _memSpan4;
    }

    public void setMemSpan5(float memSpan5)
    {
        this._memSpan5 = memSpan5;
    }

    public float getMemSpan5()
    {
        return _memSpan5;
    }

    public void setMemSpan6(float memSpan6)
    {
        this._memSpan6 = memSpan6;
    }

    public float getMemSpan6()
    {
        return _memSpan6;
    }

    public void setMemSpan7(float memSpan7)
    {
        this._memSpan7 = memSpan7;
    }

    public float getMemSpan7()
    {
        return _memSpan7;
    }

    public void setMemSpan8(float memSpan8)
    {
        this._memSpan8 = memSpan8;
    }

    public float getMemSpan8()
    {
        return _memSpan8;
    }

    public void setMemSpan9(float memSpan9)
    {
        this._memSpan9 = memSpan9;
    }

    public float getMemSpan9()
    {
        return _memSpan9;
    }

    public void setMemSpan10(float memSpan10)
    {
        this._memSpan10 = memSpan10;
    }

    public float getMemSpan10()
    {
        return _memSpan10;
    }

    public String toString ( )
    {
        String output;
        output = "Memory Span Info:\n";
        output += "_minGrValueBias = " + _minGrValueBias + "\n" + "_npairBiasSelect = " + _npairBiasSelect +"\n";
        output += "_npairSvarUpdate = " + _npairSvarUpdate + "\n" + "_stdCut = " + _stdCut + "\n";
        output += "_lagCut = " + _lagCut + "\n" + "_initSpan = " + _initSpan + "\n";
        output += "_biasQcOpt = " + _biasQcOpt + "\n" + "_numSpan = " + _numSpan + "\n";
        output += "_memSpan1 = " + _memSpan1 + "\n" + "_memSpan2 = " + _memSpan2 + "\n";
        output += "_memSpan3 = " + _memSpan3 + "\n" + "_memSpan4 = " + _memSpan4 + "\n";
        output += "_memSpan5 = " + _memSpan5 + "\n" + "_memSpan6 = " + _memSpan6 + "\n";
        output += "_memSpan7 = " + _memSpan7 + "\n" + "_memSpan8 = " +  _memSpan8 + "\n";
        output += "_memSpan9 = " + _memSpan9 + "\n" + "_memSpan10 = " + _memSpan10 + "\n";
                
        return output;
    }

}
