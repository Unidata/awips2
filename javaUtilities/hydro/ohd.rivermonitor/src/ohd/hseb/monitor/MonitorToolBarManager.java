package ohd.hseb.monitor;

import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.util.AppsDefaults;

public class MonitorToolBarManager
{
    private AppsDefaults _appsDefaults;
    protected JButton _officeNotesButton;
    private Icon _recordedOfficeNotesIcon, _emptyOfficeNotesIcon;
    private JToolBar _toolBar;
    private OfficeNotesDataManager _officeNotesDataMgr;
    
    public MonitorToolBarManager(JToolBar toolBar, AppsDefaults appsDefaults,
                                OfficeNotesDataManager officeNotesDataMgr)
    {
       _appsDefaults = appsDefaults;
       _toolBar = toolBar;
       _officeNotesDataMgr = officeNotesDataMgr;
       
       addToolBarComponents();
    }
    
    private void addToolBarComponents()
    {
        createOfficeNotesAccessButton();
        
        _toolBar.setFloatable(false);
        _toolBar.setRollover(true);
    }
    
    public JButton getOfficeNotesButton()
    {
        return _officeNotesButton;
    }
    
    private void createOfficeNotesAccessButton()
    {
        String iconsDir = _appsDefaults.getToken("rivermon_config_dir", HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);
        
        _officeNotesButton = new JButton() ;
        _recordedOfficeNotesIcon = createIcon(iconsDir + "/OfficeNotes.GIF");
        _emptyOfficeNotesIcon = createIcon(iconsDir + "/EmptyOfficeNotes.GIF");
       
        _officeNotesButton.setBorderPainted(false);
        _officeNotesButton.setRolloverEnabled(true);
      
        _toolBar.add(_officeNotesButton);
        
        setOfficeNotesButtonIcon(_officeNotesDataMgr.getCountOfOfficeNotes());
    }
     
    public Icon createIcon(String fileName)
    {
        Icon icon = null;
        int width = 25;
        int height = 25;
        Image image = Toolkit.getDefaultToolkit().getImage(fileName);
        Image newImage = image.getScaledInstance(width, height, Image.SCALE_SMOOTH);
        icon = new ImageIcon(newImage);    
        return icon;
    }
    
    public void setOfficeNotesButtonIcon(int numOfOfficeNotesRecs)
    {
        _officeNotesButton.setVisible(false);
        if(numOfOfficeNotesRecs > 0)
        {
            _officeNotesButton.setToolTipText("You have " + numOfOfficeNotesRecs + " OfficeNotes");
            _officeNotesButton.setIcon(_recordedOfficeNotesIcon);
        }
        else
        {
            _officeNotesButton.setToolTipText("OfficeNotes is Empty");
            _officeNotesButton.setIcon(_emptyOfficeNotesIcon);
        }
        _officeNotesButton.setVisible(true);
    }
    
}
