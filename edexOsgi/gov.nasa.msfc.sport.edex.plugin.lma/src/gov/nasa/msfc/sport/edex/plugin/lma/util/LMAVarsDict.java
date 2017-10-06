package gov.nasa.msfc.sport.edex.plugin.lma.util;
import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;



public class LMAVarsDict {
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LMAVarsDict.class);

    private static LMAVarsDict instance = null;
    public static final String LMA_VARS_FILE = "lma/lmaVars.xml";
    LinkedHashMap<String,LmaVar> dictVarToLmaVar;
    
    public LMAVarsDict() {
            dictVarToLmaVar = new LinkedHashMap<String,LmaVar>();
           loadVarsFiles();
    }

    public static LMAVarsDict getInstance() {
        if (instance == null) {
            instance = new LMAVarsDict();
        }
        return instance;
    }
    
    public boolean isVarSupported(String var) {
        return dictVarToLmaVar.containsKey(var);
    }
    
    public String getVarForStorage(String var) {
        return dictVarToLmaVar.get(var).getAvar();
    }
    
    public String getVarNameForStorage(String var) {
        return dictVarToLmaVar.get(var).getLongname();
    }
    
    
    private void loadVarsFiles() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> tieredMap = pathMgr
                .getTieredLocalizationFile(LocalizationType.EDEX_STATIC,
                        LMA_VARS_FILE);

        LocalizationLevel[] levels = pathMgr.getAvailableLevels();
        for (LocalizationLevel level : levels) {
            LocalizationFile file = tieredMap.get(level);
            if (file != null) {
                try {
                    loadFile(file.getFile());
                } catch (Throwable e) {
                    statusHandler.error("Unable to load site lma vars"
                            + file.getFile().getAbsolutePath(), e);
                }
            }
        }
    }
    
    
    private void loadFile(File file) {
        if (file == null || !file.exists()) {
            return;
        }
        LmaVarsContainer list = JAXB.unmarshal(file,
                LmaVarsContainer.class);
        for (LmaVar var : list.getLmaVars()) {
            dictVarToLmaVar.put(var.getName(), var);
        }
        statusHandler.debug("Successfully loaded lma vars from "
                + file.getAbsolutePath());
    }
    
}
