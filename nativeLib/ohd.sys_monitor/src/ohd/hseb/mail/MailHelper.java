package ohd.hseb.mail;

import java.util.Date;
import java.util.Properties;
import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class MailHelper
{ 
    private Properties _props = null;
    
    //----------------------------------------------------------------------------------------
    public MailHelper()
    { 
        try
        {
            _props = new Properties();
            _props.put("mail.transport.protocol", "smtp");
            _props.put("mail.smtp.host", "");
            _props.put("mail.smtp.port", 25);   
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    //----------------------------------------------------------------------------------------
    
    public boolean send(String fromAddressString,
                        String toAddressString, 
                        String subjectString,
                        String messageText)
    {
        boolean success = false;
        
        Session mailSession = Session.getInstance(_props);
        Message message = new MimeMessage(mailSession);
        
        Date sentDate = new Date();
        
        try
        {
            InternetAddress fromAddress = new InternetAddress(fromAddressString);
            message.setFrom(fromAddress);
         
            InternetAddress[] toAddressArray = InternetAddress.parse(toAddressString);
            message.setRecipients(Message.RecipientType.TO, toAddressArray);
            
            message.setSentDate(sentDate);
            message.setSubject(subjectString);
            
            message.setText(messageText);
            Transport.send(message);
            
            success = true;
        }

        catch (Exception e)
        {   
            e.printStackTrace();
        }
        
        return success;
    }
    
    //----------------------------------------------------------------------------------------
    
    public void test()
    {  
    
        String fromAddressString = "whfs@lx4-nhdr.nws.noaa.gov";
        String toAddressString = "Chip.Gobs@noaa.gov";
        String subjectString = "MailHelper Test message";
        Date sentDate = new Date();
        String messageText = "Chip,\nThis is a test message from Java Mail.\n\nChip";
           
        send (fromAddressString, toAddressString, subjectString, messageText);
        
        return;
    }
    
    //----------------------------------------------------------------------------------------
    
    public static void main(String[] args)
    {
        
        MailHelper mailHelper = new MailHelper();
        
        mailHelper.test();
        System.out.println("End of mail test.");
        
        return;
    }
    //----------------------------------------------------------------------------------------
    
}
