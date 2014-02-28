

class InsertNode(var node_id_array_index: Int, var node_id_manager : NodeIDManager, var node_list : NodeList) extends Runnable
{
   def run()
   {
       var node_id = node_list.createNode()
       
       node_id_manager.InsertNodeID(node_id_array_index, node_id)
   }

}

class NodeIDManager(var node_list : NodeList, var node_id_max : Int)
{
    var _node_id_array = new Array[Int](node_id_max)
    var _node_id_array_index = 0

    def init()
    {
        
        for (i <- 0 until node_id_max)
        {
            _node_id_array(i) = i
        }

        _node_id_array_index = 0
    }

   def InsertNodeID(node_id_array_index : Int, node_id : Int)
   {
       _node_id_array(node_id_array_index) = node_id
   }

    def getNodeID() : Int=
    {
        this.synchronized
        {
            var id = _node_id_array(_node_id_array_index)
            while( id == -1)
            {
                _node_id_array_index += 1
                _node_id_array_index = _node_id_array_index % node_id_max
            }
            var node = node_list.getNodeById(id)
            if(node.isFull())
            {
                (new Thread(new InsertNode(_node_id_array_index, this, node_list))).start()
                _node_id_array(_node_id_array_index) = -1;
            }
            _node_id_array_index += 1
            _node_id_array_index = _node_id_array_index % node_id_max
            id
       }
    }


}
