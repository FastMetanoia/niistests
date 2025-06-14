



def roots(
           k:Int,

           arms:Set[ARM],
           signalShotMapping: SignalShotMapping,
           clustersTested:Seq[VCluster] = Seq.empty
         ):Seq[VCluster] = {
  val vClusterK = getClustersOfSize(k, arms, signalShotMapping)
  for{
    c <- vClusterK
    // k - q >= 0
  } yield ???
  ???
}