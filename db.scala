import scala.xml._
import scala.collection.mutable.Map
import java.io._
    class Node() 
  {
    val id = 0
      val maxArrayLength = 20000
      val data = new Array[String](maxArrayLength)
    var currentId = 0

    def size() : Int = 
    {
      return maxArrayLength
    }

      def getIndex(in: String) : Int = 
    {
          val data = in.toInt
          val index = data % maxArrayLength
          return index
      }

      def getID() : Int = 
    {
          return id
      }
    
    def isFull() : Boolean =
    {

      return (currentId == (maxArrayLength - 1))  
    }

    def insert(url : String) : Int = 
    {
      if(isFull())
      {
        return -1
      }
      else
      {
        var temp_id = currentId
        data(temp_id) = url;
        currentId += 1
        return temp_id
      }
    }
      
    def load(fileName: String) 
    {
          val xmlFile = XML.load(fileName)
          xmlFile match 
      { 
            case <Nodes>{allNode @ _*}</Nodes> =>
            for(allAddress @ <address>{_*}</address> <- allNode)
        {
                val index = getIndex((allAddress \ "@short").text)
                data(index) = (allAddress \ "long").text
              }
          }
      }
    
    class Address(short: String, long: String) 
    {
          def toXml =
            <address short={short}>
              <long>{long}</long>
            </address>
      }

    class LockedObject()
    {

    }

      def algorithm(index: Int, nodeID: Int) : String = 
    {
          "%01d".format(nodeID) + "%02d".format(index)
      }

      def save(fileName: String) 
    {
          var addresses = Array[Address]()

          while (data.find(_ ne null) != None)
          { 
            val long = data.find(_ ne null).get
            val dataIndex = data.indexOf(long)
            val short = algorithm(dataIndex, id)
            val address = new Address(short, long)
            addresses :+= address
            data(dataIndex)=null
          }

      val newXmlFile =
            <Nodes>
            {
                { addresses.map(_.toXml) }
            }
            </Nodes>

          val printer = new scala.xml.PrettyPrinter(80, 2)
          val output = printer.format(newXmlFile)
          scala.xml.XML.save(fileName, XML.loadString(output))
      }
      def elementNumber() : Int =
      {
      return currentId
      }

  }

    class NodeList(var node_number : Int)
  {
    var nodeId = 0;
      var nodeMap = Map[Int, Node]()

      def init()
      {
        for (i <- 0 until node_number)
        {
          createNode()
        }
      }

      def loadNodeList(dir: String) 
    {
          val files = new java.io.File(dir).list.filter(_.endsWith(".xml"))
          val nodes = new Array[Node](files.length)
          for(i <- 0 until files.length)
      {
            val node = new Node()
            val inFile = dir + "/" + files(i)
            node.load(inFile)

        val nodeId : Int = files(i).substring(0, files(i).indexOf('.')).toInt
        nodeMap += (nodeId -> node)
          }
      }

      def createNode() : Int = 
    {
      val id = nodeId
          val node = new Node()
      this.synchronized
      {
        nodeMap += (nodeId -> node)
        nodeId += 1
          }
      return id;
      }
      
    def saveNodeList(dir: String) 
    {
      nodeMap.foreach(k => {
            val outFile = dir + "/" + k._1.toString + ".xml"
          println(outFile)
        nodeMap(k._1).save(outFile)
      })
      }
    
    def getNodeById(nodeId : Int) : Node = 
    {
      return nodeMap(nodeId); 
    }

      def getData(nodeId: Int, index: Int) : String = 
    {
          return getNodeById(nodeId).data(index)
      }

      def travelNodeList() : Long =
{
var number : Long = 0
nodeMap.foreach(k => {
number += nodeMap(k._1).elementNumber()
})

return number
}

def saveRuntimeParameter(transactionId : Long, shortUrlNumber : Long, cpuUsage : Double, memUsage : Double)
{
var fileName = "runTimeParameter.log"
var fw = new java.io.FileWriter(fileName, true)
val out = new PrintWriter(fw)
out.print("Usage : ")
out.println(cpuUsage)

out.print("memUsage : ")
out.println(memUsage)

out.print("totalQueryNumber : ")
out.println(transactionId)

out.print("totalshortUrlNumber : ")
out.println(shortUrlNumber)
out.println("----------------------------------")

out.close()
fw.close();
}

    }
