package ohd.hseb.monitor.settings;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import ohd.hseb.util.SessionLogger;

public abstract class MonitorSettingsFileManager
{
    protected SessionLogger _logger;
    
    protected FilterSettings _filterSettings;
    protected ViewSettings _viewSettings;
    
    protected Set<String> _displayTokenSet;
    protected Set<String> _menuTokenSet;
    
    protected List<String> _tableDisplaySettingsList;
    protected List<String> _pathTokenValuesList;
    protected List<String> _expandPathTokenValuesList;
    
    protected String _settingsFileBeginTag;
    
    public MonitorSettingsFileManager(SessionLogger logger, String settingsFileBeginTag,
            ViewSettings viewSettings,
            FilterSettings filterSettings)
    {
        _logger = logger;
        _settingsFileBeginTag = settingsFileBeginTag;
        _viewSettings = viewSettings;
        _filterSettings = filterSettings;
    }

    public Set createDisplayTokenSet()
    {
        _displayTokenSet = new HashSet<String>();

        _displayTokenSet.add(FilterSettings.PATH_TAG);
        _displayTokenSet.add(FilterSettings.EXPAND_PATH_TAG);
        _displayTokenSet.add(ViewSettings.COLUMN_TAG);
        _displayTokenSet.add(ViewSettings.SORT_TAG);
        
        return _displayTokenSet;
    }
    
    public boolean isValidSettingsFile(SessionLogger logger, File fileHandler, String checkString)
    {
        boolean isValidFile = true;
        String header = "SettingsFileManager.isValidSettingsFile(): ";
        if(fileHandler.exists())
        {
            BufferedReader in = null;
            String str;
            try
            {
                in = new BufferedReader(new FileReader(fileHandler));

                if(( str = in.readLine()) != null)
                {
                    if(!str.equals(checkString))
                    {
                        isValidFile = false;
                    }
                }
                else
                    isValidFile = false;
            }
            catch(IOException exception)
            {
                printFileException(logger, exception, header);
                isValidFile = false;
            }
        }
        else
        {
            isValidFile = false;
            logger.log(header + "[" + fileHandler.getAbsolutePath() +"] doesn't exist" ); 
        }

        return isValidFile;
    }
    
    public void printFileException(SessionLogger logger, IOException exception, String header)
    {
        header = header+".printFileException(): ";
        logger.log(header+"ERROR = " + exception.getMessage());
        exception.printStackTrace(logger.getPrintWriter());
        logger.log(header+"End of stack trace");
    }
    
    public void saveDisplaySettings(BufferedWriter out)
    {
        String columnDisplaySettings = _viewSettings.getColumnDisplaySettings();
        saveTableDisplaySettings(out, columnDisplaySettings);

        //Write the selected paths
        TreePath[] selectedPaths = _filterSettings.getSelectedPaths();

        saveTreeSettings(out, selectedPaths, FilterSettings.PATH_TAG);

        //Write the expanded paths
        TreePath[] expandedPaths = _filterSettings.getExpandedPaths();

        saveTreeSettings(out, expandedPaths, FilterSettings.EXPAND_PATH_TAG);
    }
    
    public void createSettingsFile(File fileHandler)
    {
        boolean fileExist = false;
        fileExist = fileHandler.exists();
        String header = "RiverMonitorApplicationSettingsManager.createSettingsFile(): ";

        if(!fileExist)
        {
            try
            {
                fileHandler.createNewFile();
            }
            catch(IOException e)
            {
                _logger.log(header+ "Unable to Create file:"+
                    fileHandler.getAbsolutePath()+ 
                " for saving table settings");  
            }
        }
        else
        {
            boolean fileDelete = false;
            fileDelete = fileHandler.delete();

            if(!fileDelete)
            {
                _logger.log(header+"FileName [" + fileHandler.getAbsolutePath()+
                    "]exists already" +
                    " and unable to delete inorder to" + 
                "create a new file");
            }
            else
            {
                try
                {
                    fileHandler.createNewFile();
                }
                catch(IOException exception)
                {
                    _logger.log(header+"Unable to Create file:"+
                        fileHandler.getAbsolutePath()+
                    " for saving column setting");
                    _logger.log(header+"ERROR = " + exception.getMessage());
                    exception.printStackTrace(_logger.getPrintWriter());
                }
            }
        }
    }
    
    public FilterSettings getFilterSettings()
    {
        return _filterSettings;
    }

    public ViewSettings getViewSettings()
    {
        return _viewSettings;
    }

    public boolean isSettingsLine(String line)
    {
        boolean result = false;

        if(line.length() > 0)
        {
            if(line.charAt(0) != '\n')
            {
                if(line.charAt(0) != '#')
                {
                    result = true;
                }
            }
        }
        return result;
    }
    
    public void processTableDisplayToken(String line)
    {
        if(_tableDisplaySettingsList == null)
        {
            _tableDisplaySettingsList = new ArrayList<String>();
        }

        _tableDisplaySettingsList.add(line);
    }

    public void processPathToken(String tokenValue)
    {
        if(_pathTokenValuesList == null)
        {
            _pathTokenValuesList = new ArrayList<String>();
        }

        if(tokenValue.length() > 0 && tokenValue.charAt(0) != '\n')
        {
            tokenValue = tokenValue.replace('[', ' ').replace(']', ' ').trim();
            _pathTokenValuesList.add(tokenValue);
        }
    }

    public void processExpandPathToken(String tokenValue)
    {
        if(_expandPathTokenValuesList == null)
        {
            _expandPathTokenValuesList = new ArrayList<String>();
        }
        if(tokenValue.length() > 0 && tokenValue.charAt(0) != '\n')
        {
            tokenValue = tokenValue.replace('[', ' ').replace(']', ' ').trim();
            _expandPathTokenValuesList.add(tokenValue);
        }
    }

   
    public void processDisplayToken(String tokenName, String tokenValue)
    {
        if((tokenName.compareTo(ViewSettings.COLUMN_TAG) == 0)
                || (tokenName.compareTo(ViewSettings.SORT_TAG) == 0))
        {
            processTableDisplayToken(tokenName + ":" + tokenValue);
        }
        else if(tokenName.compareTo(FilterSettings.PATH_TAG) == 0) 
        {
            processPathToken(tokenValue);
        }
        else if(tokenName.compareTo(FilterSettings.EXPAND_PATH_TAG) == 0) 
        {
            processExpandPathToken(tokenValue);
        }
    }
    
    public void processLine(String line)
    {
        String tokenName;
        String tokenValue;

        if(isSettingsLine(line))
        {
            String[] splitString = line.split(":");

            if(splitString != null)
            {
                if( splitString.length == 2 )
                {
                    tokenName = splitString[0].trim();
                    tokenValue = splitString[1].trim();
                    if(_menuTokenSet.contains(tokenName))
                    {
                        processMenuToken(tokenName, tokenValue);
                    }
                    else if(_displayTokenSet.contains(tokenName))
                    {
                        processDisplayToken(tokenName, tokenValue);
                    } // token is a valid token

                } // is a valid line line str1:str2

            } // line has the delimiter :

        }// is a valid settings line

    }

    public void processMenuToken(String tokenName, String tokenValue)
    {
        
    }
    
    public TreePath[] getTreePathArray(List<String> pathList)
    {
        TreePath[] paths = null;
        if(pathList != null)
        {
            paths = new TreePath[pathList.size()];
            for(int i=0; i < pathList.size(); i++)
            {
                String splitStr[] = pathList.get(i).toString().split(", ");
                DefaultMutableTreeNode node[] = new DefaultMutableTreeNode[splitStr.length];
                for(int j=0; j < splitStr.length; j++)
                {
                    node[j] = new DefaultMutableTreeNode(splitStr[j]);
                }
                paths[i] = new TreePath(node);

            }
        }
        return paths;
    }

    public void setViewAndFilterSettings()
    {
        String header = "MonitorSettingsFileManager.setViewAndFilterSettings(): ";
        System.out.println(header);
        
        String tableDisplaySettingsString = null;
        
        if(_tableDisplaySettingsList != null)
        {
            for(int i=0; i < _tableDisplaySettingsList.size(); i++)
            {
                String str = _tableDisplaySettingsList.get(i);
                if(tableDisplaySettingsString == null)
                    tableDisplaySettingsString = str +"\n";
                else
                    tableDisplaySettingsString = tableDisplaySettingsString + str +"\n";
            }
        }
        
        //TODO CHIP says: a StringBuffer should be used instead of tableDisplaySettingsString, which will create a lot of String objects
        //
       // System.out.println(header + "tableDisplaySettingsString = \n" + tableDisplaySettingsString);
        _viewSettings.setColumnDisplaySettings(tableDisplaySettingsString);

        TreePath[] paths = getTreePathArray(_pathTokenValuesList);
        _filterSettings.setSelectedPaths(paths);   

        paths = getTreePathArray(_expandPathTokenValuesList);
        _filterSettings.setExpandedPaths(paths);   
    }
    
    public void saveTableDisplaySettings(BufferedWriter out, String columnDisplaySettings)
    {
        String header = "DisplaySettingsFileManager.saveTableDisplaySettings(): ";

        if(columnDisplaySettings != null)
        {
            for(int i=0; i < columnDisplaySettings.length(); i++)
            {
                try
                {
                    if(i==0)
                    {
                        out.write(_settingsFileBeginTag);
                        out.newLine();
                    }
                    if(columnDisplaySettings.charAt(i) == '\n')
                        out.newLine();
                    else
                        out.write(columnDisplaySettings.charAt(i));
                }
                catch(IOException exception)
                {
                    printFileException(_logger, exception,header);
                    return;
                }
            }
        }

    }

    public void saveTreeSettings(BufferedWriter out, TreePath paths[], String settingsTag)
    {
        String header = "DisplaySettingsFileManager.saveFilterTreeSettings(): ";

        if(paths == null)
        {
            _logger.log(header + " Paths sent are null while Trying to save " + settingsTag);
            return;
        }

        for(int j=0; j < paths.length; j++)
        {
            try
            {
                if(j == 0)
                {
                    out.newLine();   
                    out.write(FilterSettings.TREE_SETTINGS_BEGIN_TAG);
                    out.newLine();
                }
                out.write(settingsTag);
                out.write(":");
            }
            catch(IOException exception)
            {
                printFileException(_logger,exception,header);
            }

            String pathStr = paths[j].toString();
            for(int i=0; i < pathStr.length(); i++)
            {
                try
                {
                    if(i==pathStr.length()-1)
                    {
                        out.write(pathStr.charAt(i));
                        out.newLine();
                    }
                    else
                        out.write(pathStr.charAt(i));
                }
                catch(IOException exception)
                {
                    printFileException(_logger,exception,header);
                    return;
                }
            }
        }
    }


}
