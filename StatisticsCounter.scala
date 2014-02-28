import java.io._

class StatisticsCounter(var server : HttpServer, var node_list : NodeList) extends Runnable
{
   def getCpuUsage() : Double=
   {
       return 0.42
   }
  
   def getMemUsage() : Double=
   {
       return 0.1
   }

   def store()
   {
       val transaction_number = server.getTransactionNumber()
       val records_number = node_list.travelNodeList()
       val cpu_usage = getCpuUsage()
       val mem_usage = getMemUsage()
           
       node_list.saveRuntimeParameter(transaction_number, records_number, cpu_usage, mem_usage)
   }

   def run()
   {
       while (true)
       {
           Thread.sleep(5000)
           store()
       }
   }
}
