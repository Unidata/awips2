package ohd.hseb.sshp;

import java.util.ArrayList;
import java.util.List;

public class SSHPSource
{
    // -----------------------------------------------------------------------------------------
    
    public final static SSHPSource ADJUSTED_VAR =   new SSHPSource("adjusted var"  , "ADJ_VAR"  , "FA", "ADJRUNOFF");
    public final static SSHPSource UNADJUSTED_VAR = new SSHPSource("unadjusted var", "UNADJ_VAR", "FU", "UADJRUNOFF");
      
    public final static SSHPSource RFC          = new SSHPSource("RFC", "RFC", "FZ", "RFCRUNOFF");
    public final static SSHPSource SSHP_UPDATER = new SSHPSource("SSHP_Updater", "LOCAL", "FZ", "SSHPRUNOFF" );

    public final static SSHPSource GENE_VAR = new SSHPSource("GENETIC VAR", "GENE_VAR", "FG", "GENERUNOFF" );
   // public final static SSHPSource GENE_VAR = new SSHPSource("GENETIC VAR", "GENE_VAR", "FA", "ADJRUNOFF" );

    
    private String _name = null;
    private String _productId = null;
    private String _sacSmaSource = null;  //this is to be used for SacSmaStates
    private String _typeSource = null;    //this is to be used for TimeSeries info such as PriorComputerRunoff
    
    // -----------------------------------------------------------------------------------------
    
    private static List<SSHPSource> _sourceList = new ArrayList();
    
    static 
    {
        _sourceList.add(ADJUSTED_VAR);
        _sourceList.add(UNADJUSTED_VAR);
        _sourceList.add(RFC);
        _sourceList.add(SSHP_UPDATER);  
        _sourceList.add(GENE_VAR);  
    }
    // -----------------------------------------------------------------------------------------
     
    private SSHPSource(String name, String sacSmaSource, String typeSource, String productId)
    {
        setName(name);
        setSacSmaSource(sacSmaSource);
        setTypeSource(typeSource);
        setProductId(productId);
    }
    // -----------------------------------------------------------------------------------------
     
    public static SSHPSource findMatchingSSHPSourceBySacSmaSource(String sacSmaSource)
    {
        String header = "SSHPSource.findMatchingSSHPSourceBySacSmaSource(): ";
        
        SSHPSource matchingSource = null;
        
        for ( SSHPSource source : _sourceList)
        {
            if (sacSmaSource.equals(source.getSacSmaSource()))
            {
                matchingSource = source;
                break;
            }
        }
     
        if (matchingSource != null)
        {
            System.out.println(header + " matchingSource = " + matchingSource.getSacSmaSource() );
        }
        
        return matchingSource;
    }

    // -----------------------------------------------------------------------------------------
    
    public void setName(String name)
    {
        _name = name;
    }

    // -----------------------------------------------------------------------------------------

    public String getName()
    {
        return _name;
    }

    // -----------------------------------------------------------------------------------------
    
    public void setSacSmaSource(String sacSmaSource)
    {
        _sacSmaSource = sacSmaSource;
    }
    
    // -----------------------------------------------------------------------------------------
    
    public String getSacSmaSource()
    {
        return _sacSmaSource;
    }
    
    // -----------------------------------------------------------------------------------------
    
    public void setTypeSource(String typeSource)
    {
        _typeSource = typeSource;
    }

    // -----------------------------------------------------------------------------------------
    
    public String getTypeSource()
    {
        return _typeSource;
    }
    // -----------------------------------------------------------------------------------------

    public void setProductId(String productId) {
        _productId = productId;
    }

    // -----------------------------------------------------------------------------------------

    public String getProductId() {
        return _productId;
    }
    
}
