function RetrieveHibStats(){
  this.cat = new HibernateStats();
}
function _execute(){
  return this.cat.execute();
}

RetrieveHibStats.prototype.execute = _execute;