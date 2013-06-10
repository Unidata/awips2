package com.raytheon.viz.mpe.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.common.CommandException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.ui.actions.SetDisplayField;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Abstract dynamic fields populator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public abstract class FieldsPopulator extends CompoundContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FieldsPopulator.class);

    protected static class MenuData {
        String text;

        String mnemonic;

        int type;

        String handler;

        Map<?, ?> params;

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         */
        public MenuData(String text, String mnemonic) {
            this(text, mnemonic, Action.AS_PUSH_BUTTON);
        }

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         * @param type
         *            The Action type like &lt;Action.AS_PUSH_BUTTON&gt;
         */
        public MenuData(String text, String mnemonic, int type) {
            this(text, mnemonic, type, null, null);
        }

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         * @param type
         *            The Action type like &lt;Action.AS_PUSH_BUTTON&gt;
         * @param handler
         *            The handler id defined in plugin.xml
         * @param params
         *            The parameters of the handler
         */
        public MenuData(String text, String mnemonic, int type, String handler,
                Map<?, ?> params) {
            this.text = text;
            this.mnemonic = mnemonic;
            this.type = type;
            this.handler = handler;
            this.params = params;

        }
    }

    @Override
    protected IContributionItem[] getContributionItems() {
        AppsDefaults defaults = AppsDefaults.getInstance();
        String list = defaults.getToken("mpe_generate_list");
        List<String> fields = new ArrayList<String>();
        if (list != null) {
            fields = new ArrayList<String>(Arrays.asList(list.split("[,]")));
            Collections.sort(fields);
        }
        for (DisplayFieldData data : getMenuItems()) {
            final int found = Collections.binarySearch(fields, data.name(),
                    new Comparator<String>() {
                        @Override
                        public int compare(String o1, String o2) {
                            return o1.compareToIgnoreCase(o2);
                        }
                    });
            boolean enabled = found >= 0;
            ActionContributionItem itemFound = (ActionContributionItem) getMenuManger()
                    .find(getTexMap().get(data).handler);
            if (itemFound == null) {
                itemFound = (ActionContributionItem) getMenuManger().find(
                        getTexMap().get(data).text);
            }
            if (itemFound == null) {
                getMenuManger().add(
                        new ActionContributionItem(new DisplayFieldAction(data,
                                enabled)));
            } else {
                itemFound.getAction().setEnabled(enabled);
            }
        }

        return getMenuManger().getItems();
    }

    protected class DisplayFieldAction extends Action {
        private final DisplayFieldData data;

        private final MenuData menuData;

        public DisplayFieldAction(DisplayFieldData data, boolean enabled) {
            super("", getTexMap().get(data).type);
            this.menuData = getTexMap().get(data);
            String id = this.menuData.handler == null ? this.menuData.text
                    : this.menuData.handler;
            this.setActionDefinitionId(this.menuData.handler);
            this.setId(id);
            String s = getTexMap().get(data).text;
            String mnemonic = getTexMap().get(data).mnemonic;
            if (mnemonic == null || s == null) {
                setText(s);
            }
            int idx = s.indexOf(mnemonic);
            if (idx == -1) {
                setText(s);
            } else {
                setText(s.substring(0, idx) + '&' + s.substring(idx));
            }
            this.data = data;
            setEnabled(enabled);
        }

        @Override
        public void run() {
            if (menuData.handler == null) {
                SetDisplayField.setDisplayField(
                        EditorUtil.getActiveVizContainer(), data);
            } else {
                try {
                    ICommandService commandService = (ICommandService) PlatformUI
                            .getWorkbench().getService(ICommandService.class);
                    Command command = commandService
                            .getCommand(menuData.handler);
                    if (command != null) {
                        ExecutionEvent event = new ExecutionEvent(command,
                                menuData.params, null, null);
                        command.executeWithChecks(event);
                    }
                } catch (CommandException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error excuting command", e);
                }
            }
        }

    }

    /**
     * @return The dynamic menu items
     */
    protected abstract DisplayFieldData[] getMenuItems();

    /**
     * @return The menu to be altered
     */
    protected abstract MenuManager getMenuManger();

    /**
     * @return The map of MenuData items
     */
    protected abstract Map<DisplayFieldData, MenuData> getTexMap();

}
