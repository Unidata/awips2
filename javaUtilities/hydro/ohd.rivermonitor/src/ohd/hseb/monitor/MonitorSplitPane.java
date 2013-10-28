package ohd.hseb.monitor;

import java.awt.Component;
//import java.awt.Graphics;

import javax.swing.JSplitPane;

public class MonitorSplitPane extends JSplitPane
{

    public MonitorSplitPane (int newOrientation,
            boolean newContinuousLayout)
    {
        super(newOrientation, newContinuousLayout);
    }

    public void setDividerLocation(int location)
    {
        int currentDividerLocation = getDividerLocation();
        int adjustedLocation = location;


        if (location < 30 )
        {
            int difference = currentDividerLocation - location;
            if (difference > 50)
            {
                adjustedLocation = currentDividerLocation;
            }
        }
        super.setDividerLocation(adjustedLocation);
    }

    public void setRightComponent(Component comp)
    {
        String header = "MonitorJSplitPane.setRightComponent() ";

        int beforeDividerLocation = getDividerLocation();
        
        

        super.setRightComponent(comp);

        //int afterDividerLocation = getDividerLocation();

        setDividerLocation(beforeDividerLocation);

        //  System.out.println(header + " before divider location = " + beforeDividerLocation);
        //  System.out.println(header + "  after divider location = " + afterDividerLocation);
 
        //setVisible(false);
        //setVisible(true);
        
    }

    public void setLeftComponent(Component comp)
    {
        String header = "MonitorJSplitPane.setLeftComponent() ";

        int beforeDividerLocation = getDividerLocation();
    
        super.setLeftComponent(comp);

        //int afterDividerLocation = getDividerLocation();

        setDividerLocation(beforeDividerLocation);

       // System.out.println(header + " before divider location = " + beforeDividerLocation);
       // System.out.println(header + "  after divider location = " + afterDividerLocation);

    }

    /*
    public void paint(Graphics g)
    {
        String header = "MonitorJSplitPane.paint() ";
        super.paint(g);

        int dividerLocation = getDividerLocation();
        int lastDividerLocation = getLastDividerLocation();

        System.out.println(header + " divider location = " + dividerLocation);
        System.out.println(header + " last divider location = " + lastDividerLocation);

    }
    */
}

