package com.raytheon.viz.ui.personalities.awips;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;

/**
 * Listener to track command execution in CAVE, and output command execution
 * time
 * 
 * get ICommandService and register IExecutionListener as following:
 * ICommandService service = (ICommandService)
 * PlatformUI.getWorkbench().getService(ICommandService.class);
 * service.addExecutionListener(new CommandExecutionListener());
 * 
 * Since the above call can only be made after workbench is initialized, so the
 * best place to call them would be in WorkbenchAdvisor#postStartup(). For CAVE,
 * the class is VizWorkbenchAdvisor
 * 
 * @author Wufeng Zhou
 * 
 */
public class CaveCommandExecutionListener extends
        AbstractCavePerformanceMonitor implements IExecutionListener {

    private static final CaveCommandExecutionListener instance = new CaveCommandExecutionListener();
    
    private Properties cmdDescProps = new Properties(); 
    
    public static CaveCommandExecutionListener getInstance() {
        return instance;
    }

    private CaveCommandExecutionListener() {
        super("caveCommandLog");
    }

    @Override
    public void notHandled(String commandId, NotHandledException exception) {
        String key = commandId + "__" + Thread.currentThread().getId() + "__"
                + Thread.currentThread().getName();
        if (startTimeMap.get(key) == null) {
            log("notHandled: Fail to find start time for " + key);
            log(exception);
        } else {
            Long startTime = startTimeMap.get(key);
            startTimeMap.remove(key);
            String cmdDesc = cmdDescProps.getProperty(key, key);
            cmdDescProps.remove(key);
            long delta = System.currentTimeMillis() - startTime.longValue();
            log("notHandled: " + cmdDesc + " executed in " + delta
                    + "ms with exception: " + exception.getMessage());
            log(exception);
        }
    }

    @Override
    public void postExecuteFailure(String commandId,
            ExecutionException exception) {
        String key = commandId + "__" + Thread.currentThread().getId() + "__"
                + Thread.currentThread().getName();
        if (startTimeMap.get(key) == null) {
            log("postExecuteFailure: Fail to find start time for "
                    + key);
            log(exception);
        } else {
            Long startTime = startTimeMap.get(key);
            startTimeMap.remove(key);
            String cmdDesc = cmdDescProps.getProperty(key, key);
            cmdDescProps.remove(key);
            long delta = System.currentTimeMillis() - startTime.longValue();
            log("postExecuteFailure: " + cmdDesc + " executed in " + delta
                    + "ms with exception: " + exception.getMessage());
            log(exception);
        }
    }

    @Override
    public void postExecuteSuccess(String commandId, Object returnValue) {
        String key = commandId + "__" + Thread.currentThread().getId() + "__"
                + Thread.currentThread().getName();
        if (startTimeMap.get(key) == null) {
            log("postExecuteSuccess: Fail to find start time for "
                    + key);
        } else {
            Long startTime = startTimeMap.get(key);
            startTimeMap.remove(key);
            String cmdDesc = cmdDescProps.getProperty(key, key);
            cmdDescProps.remove(key);
            long delta = System.currentTimeMillis() - startTime.longValue();

            // only count command execution time when success
            Integer count = runCountMap.get(commandId);
            if (count == null)
                count = new Integer(0);
            Long totalTime = runTotalTimeMap.get(commandId);
            if (totalTime == null)
                totalTime = new Long(0);
            // now the new count
            count = count + 1;
            totalTime = totalTime + delta;
            runCountMap.put(commandId, count);
            runTotalTimeMap.put(commandId, totalTime);
            long avgTime = totalTime / count;

            log("Cmd Exe: " + cmdDesc + " exe " + delta
                    + "ms with returnValue=" + returnValue + ", cnt=" + count);
        }
    }

    @Override
    public void preExecute(String commandId, ExecutionEvent event) {
        // TODO Auto-generated method stub
        String key = commandId + "__" + Thread.currentThread().getId() + "__"
                + Thread.currentThread().getName();
        String commandDesc = getCommandDescription(commandId, event);
        startTimeMap.put(key, System.currentTimeMillis());
        cmdDescProps.put(key, commandDesc);
    }

    private static String getCommandDescription(String commandId,
            ExecutionEvent event) {
        Map paramMap = event.getParameters();
        StringBuffer buf = new StringBuffer();
        buf.append("{commandId=").append(commandId).append(",");
        buf.append("params=[");
        Iterator iter = paramMap.keySet().iterator();
        boolean first = true;
        while (iter.hasNext()) {
            String key = String.valueOf(iter.next());
            String value = String.valueOf(paramMap.get(key));
            if (first == false)
                buf.append(",");
            buf.append(key).append("='").append(value).append("'");
            first = false;
        }

        buf.append("]").append("}");
        return buf.toString();
    }

}
