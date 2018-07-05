package gov.noaa.gsd.viz.ensemble.display.common;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;

/**
 * This resource holder is specifically for ensemble products which hold
 * one-to-many members (perturbation resources).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 01, 2017  26745      polster    Initial Creation.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1
 */

public class EnsembleMembersHolder extends AbstractResourceHolder {

    String ensembleName = null;

    List<AbstractResourceHolder> members = null;

    public EnsembleMembersHolder(String ensName,
            List<AbstractResourceHolder> ensMembers) {
        isGenerated = false;
        isIndividualProduct = false;
        ensembleName = ensName;
        members = ensMembers;
        for (AbstractResourceHolder arh : members) {
            arh.setParent(this);
        }
    }

    @Override
    public boolean requiresLoadCheck() {
        return false;
    }

    @Override
    public String getGroupName() {
        return ensembleName;
    }

    @Override
    public String getGeneralName() {
        return ensembleName;
    }

    @Override
    public String getSpecificName() {
        return "";
    }

    @Override
    public String getModel() {
        return "";
    }

    @Override
    public String getLocation() {
        return "";
    }

    @Override
    public String getLevel() {
        return "";
    }

    @Override
    public String getParameter() {
        return "";
    }

    @Override
    public String getUnits() {
        return "";
    }

    @Override
    public String getDataTime() {
        return "";
    }

    @Override
    public String getType() {
        return "";
    }

    @Override
    public String getEnsembleId() {
        return ensembleName;
    }

    @Override
    public String getEnsembleIdRaw() {
        return ensembleName;
    }

    @Override
    public String getStationId() {
        return "";
    }

    @Override
    public Calculation getCalculation() {
        return null;
    }

    @Override
    public boolean isEnsembleGroup() {
        return true;
    }

    @Override
    public AbstractResourceHolder[] getChildren() {
        AbstractResourceHolder[] primMembers = null;
        if (members != null && !members.isEmpty()) {
            primMembers = new AbstractResourceHolder[members.size()];
            int index = 0;
            for (AbstractResourceHolder arh : members) {
                primMembers[index++] = arh;
            }
        }
        return primMembers;
    }

    @Override
    public boolean hasChildren() {
        boolean hasChildren = false;
        if (members != null && !members.isEmpty()) {
            hasChildren = true;
        }
        return hasChildren;

    }

    @Override
    public boolean equals(Object anotherHolder) {
        boolean isEqual = false;
        if (anotherHolder instanceof EnsembleMembersHolder) {
            EnsembleMembersHolder emh = (EnsembleMembersHolder) anotherHolder;
            isEqual = ensembleName.equals(emh.getGroupName());
        }
        return isEqual;
    }

    public void addMember(AbstractResourceHolder arh) {
        if (members == null) {
            members = new CopyOnWriteArrayList<>();
        }
        members.add(arh);
    }

    @Override
    public int hashCode() {
        return ensembleName.hashCode();
    }

}
