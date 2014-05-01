package com.raytheon.viz.gfe.actions.parametervalues;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.IParameterValues;

import com.raytheon.viz.gfe.procedures.ProcedureCatalog;

public class ProcedureValues implements IParameterValues {

    @SuppressWarnings("rawtypes")
    @Override
    public Map getParameterValues() {
        Map<String, String> values = new HashMap<String, String>();

        ProcedureCatalog catalog = new ProcedureCatalog();
        for (String name : catalog.getNames()) {
            values.put(name, name);
        }

        return values;
    }

}
