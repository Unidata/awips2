package ohd.hseb.monitor;

public class TextStatus implements Status
{
    private String _text;

    
    public TextStatus(String text)
    {
        setText(text);
    }
    
    public void setText(String text)
    {
        _text = text;
    }

    public String getText()
    {
        return _text;
    }
    
    public String toString()
    {
        return getText();
    }
    
} // TestStatus
