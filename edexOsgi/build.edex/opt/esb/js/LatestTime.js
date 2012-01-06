function LatestTime(uriList){
    this.ltq = new LatestTimeQuery(uriList);
}

function _execute(){
    return this.ltq.execute();
}

LatestTime.prototype.execute = _execute;