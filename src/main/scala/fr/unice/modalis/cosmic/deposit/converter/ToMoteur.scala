package fr.unice.modalis.cosmic.deposit.converter

/**
 * Created by Cyril Cecchinel - I3S Laboratory on 02/12/14.
 */
object ToMoteur {
  /*def apply[T<:DataType](w: Workflow): String = generateCode(w)

  def generateElementCode(e: WFActivity) = {

    def generateElementIOCode[T<:DataType](io: Port[T]) = {
      io match {
        case Input(name) => <in name={name} type="integer" depth="0"/>
        case Output(name) => <out name={name} type="integer" depth="0"/>
      }
    }

    <processor name={e.id} type="integer">
      { (e.inputs).map(generateElementIOCode(_) ++ (e.outputs).map(generateElementIOCode(_))) }
    </processor>

  }

  def generateLinkCode(l: WFLink) = {
    <link from={(l.source) match {case Sensor(_) => l.source_output.name; case _ => (l.source.id + ":" + l.source_output.name)}} to={(l.destination) match {case Collector(_) => l.destination_input.name; case _ => (l.destination.id + ":" + l.destination_input.name)}}/>
  }

  def generateInterfaceCode[T<:DataType](i: DataIO[T]) = {
    i match {
      case a:Sensor[_] => <source name={a.output.name} type="integer"></source>
      case a:Collector[_] => <sink name={a.input.name} type="integer"></sink>
    }
  }



  def generateCode(w: Workflow): String = {

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      <workflow name="new workflow" version="0.1" author="unknown">
        <interface>
          {
          (w.sources ++ w.sinks).map(generateInterfaceCode(_))
          }
        </interface>
        <processors>
          {
          (w.elements -- (w.sources ++ w.sinks)).map(a => generateElementCode(a.asInstanceOf[WFActivity]))
          }
        </processors>
        <links>
          {
          w.links.map(generateLinkCode(_))
          }
        </links>
      </workflow>.toString()

  }*/
}
