package ohd.hseb.bias_trans;


public class RfcBiasMessage implements RfcBiasConstants 
{
        
    public RfcBiasMessage ( )
    {
    }
     
    int write_bias_message ( BiasDataMgr dataMgr )
    {
        
        write_bias_message_header ( dataMgr );
        
        // Retrieve the list of radars to get bias information for.
        write_bias_message_header ( dataMgr );
        write_bias_message_body ( dataMgr );
        
        return 0;
    }
    
    private int write_bias_message_header ( BiasDataMgr dataMgr )
    {
        return 0;
    }
    
    private int write_bias_message_body ( BiasDataMgr dataMgr )
    {
        return 0;
    }
      
}
