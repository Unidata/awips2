package gov.noaa.gsd.viz.ensemble.display.common;

public interface ITreeModelProvider {

    /*
     * Returns true if this instance is an ensemble group.
     */
    public boolean isEnsembleGroup();

    /*
     * If this is an ensemble group this method will return the ensemble
     * members.
     */
    public AbstractResourceHolder[] getChildren();

    /*
     * If this element is an ensemble member than this method returns the parent
     * ensemble group.
     */
    public Object getParent();

    /*
     * If this is an ensemble group this method should always return true,
     * unless for some pathological reason there are no ensemble members
     * associated with the ensemble group.
     */
    public boolean hasChildren();

    /*
     * Set the parent of a resource to be an ensemble group.
     */
    public void setParent(EnsembleMembersHolder emh);

}
