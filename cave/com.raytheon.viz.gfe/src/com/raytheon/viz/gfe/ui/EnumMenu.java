/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.gfe.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IParameter;
import org.eclipse.core.commands.IParameterValues;
import org.eclipse.core.commands.ParameterValuesException;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

/**
 * Create a menu to select one value from an enum
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class EnumMenu extends CompoundContributionItem {
    /**
     * Private class defining the command parameter for setting an enum value
     */
    private static class EnumParameter implements IParameter {
        private final EnumParameterValues values;

        EnumParameter(Enum<?> e) {
            values = new EnumParameterValues(e);
        }

        @Override
        public String getId() {
            return "value";
        }

        @Override
        public String getName() {
            return "value";
        }

        @Override
        public IParameterValues getValues() throws ParameterValuesException {
            return values;
        }

        @Override
        public boolean isOptional() {
            return false;
        }

    }

    /**
     * Private class defining the values for EnumParameter
     */
    private static class EnumParameterValues implements IParameterValues {
        private Map<String, String> valueMap = new HashMap<String, String>();

        EnumParameterValues(Enum<?> e) {
            for (Enum<?> value : e.getDeclaringClass().getEnumConstants()) {
                valueMap.put(value.toString(), value.name());
            }
        }

        @Override
        public Map<String, String> getParameterValues() {
            return valueMap;
        }

    }

    /**
     * Private class defining the handler to set an enum value
     */
    public class EnumHandler extends AbstractHandler {
        private Enum<?> e;

        EnumHandler(Enum<?> e) {
            this.e = e;
        }

        @SuppressWarnings("unchecked")
        @Override
        public Object execute(ExecutionEvent event) throws ExecutionException {
            MenuItem item = (MenuItem) ((Event) event.getTrigger()).widget;
            if (item.getSelection()) {
                setCurrentValue(Enum.valueOf(e.getClass(), event
                        .getParameter("value")));
            }
            return null;
        }

    }

    /**
     * Base constructor for an enum menu.
     * 
     * Defines the command and handler to be used by the menu
     */
    protected EnumMenu() {
    }

    /**
     * 
     */
    private void defineCommand() {
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        Category category = service.getCategory(getCategoryId());

        Command command = service.getCommand(getCommandId());

        Enum<?> e = getCurrentValue();
        IParameter[] parameters = new IParameter[] { new EnumParameter(e) };
        command.define(getCommandName(), null, category, parameters);
        command.setHandler(new EnumHandler(e));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        if (!service.getCommand(getCommandId()).isDefined()) {
            defineCommand();
        }

        // create a MenuItem for each enum value
        Enum<?>[] values = getCurrentValue().getClass().getEnumConstants();
        IContributionItem[] items = new IContributionItem[values.length];

        int i = 0;
        for (Enum<?> value : values) {
            HashMap<String, String> map = new HashMap<String, String>();
            map.put("value", value.name());
            items[i++] = new CommandContributionItem(
                    new CommandContributionItemParameter(PlatformUI
                            .getWorkbench(), value.name(), getCommandId(), map,
                            null, null, null, value.toString(), null, null,
                            CommandContributionItem.STYLE_RADIO, null, true));
        }
        return items;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#fill(org.eclipse.swt.
     * widgets.Menu, int)
     */
    @Override
    public void fill(Menu menu, int index) {
        super.fill(menu, index);

        // add a menu listener to set the currently selected menu item
        menu.addMenuListener(new MenuAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.MenuAdapter#menuShown(org.eclipse.swt.
             * events.MenuEvent)
             */
            @Override
            public void menuShown(MenuEvent e) {
                if (e.widget instanceof Menu) {
                    for (MenuItem item : ((Menu) e.widget).getItems()) {
                        item.setSelection(item.getText().equals(
                                getCurrentValue().toString()));
                    }
                }
            }

        });
    }

    /**
     * returns the currently selected value of the enum
     * 
     * @return
     */
    protected abstract Enum<?> getCurrentValue();

    /**
     * sets the currently selected value of the enum
     * 
     * @return
     */
    protected abstract void setCurrentValue(Enum<?> value);

    /**
     * returns the ID of the command to be executed
     * 
     * @return
     */
    protected abstract String getCommandId();

    /**
     * returns the name of the command to be executed
     * 
     * @return
     */
    protected abstract String getCommandName();

    /**
     * returns the Category ID of the command
     * 
     * @return
     */
    protected abstract String getCategoryId();

}
