package ohd.hseb.monitor;

import ohd.hseb.mail.MailHelper;

public class EmailReporter implements Reporter
{

    private String _fromAddressString = null;
    private String _toAddressString = null;
    private String _subjectString = null;
    
    private MailHelper _mailHelper = new MailHelper();
    
    public EmailReporter(String fromAddressString, String toAddressString, String subjectString)
    {
        _fromAddressString = fromAddressString;
        _toAddressString = toAddressString;
        _subjectString = subjectString;

            
        return;
    }

    
    public void report(Status status)
    {
     
        _mailHelper.send(_fromAddressString, _toAddressString, _subjectString, status.toString()); 
    }

} //end EmailReporter
