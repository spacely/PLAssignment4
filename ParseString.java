import java.util.*;

public class ParseString {


private String expression[];
 Map<String,String> cache = new HashMap<String,String>();
 Stack<String> opsStack = new Stack<String>();
 Stack<String> valueStack = new Stack<String>();
 Queue<String> valueList = new LinkedList<String>();







public ParseString(String expression[]){
  this.expression = expression;
  initMap();
}

void initMap(){
  cache.put("while","loop");
  cache.put("if","condition");
  cache.put("else","condition");
  cache.put(":=","ops");
  cache.put("+","ops");
  cache.put("-","ops");
  cache.put("skip","ops");

}


void tokenize(){

  for(int i=0;i<expression.length;i++){
    //if(expression.)
    if(cache.containsKey(expression[i])){
      opsStack.push(expression[i]);

    }
    else {
      valueList.add(expression[i]);

    }
  }


}

String eval(){
  StringBuilder val = new StringBuilder();
  while(!opsStack.empty()){
    String a = opsStack.pop();
    if(a.equals(":=")){
      val.append("\u21D2 skip, ");
      val.append("{");
      String x = valueList.remove();
      String y = valueList.remove();
      val.append(x+" ");
      val.append("\u2192"+" ");
      val.append(y);
      val.append("}");
    }
    if(a.equals("skip")){
      val.append("");
    }
  }



return val.toString();
}

public static void main(String args[]){

  Scanner scanner = new Scanner(System.in);
    try {


    while(scanner.hasNextLine()){

    StringBuilder sb =  new StringBuilder(scanner.nextLine());
    sb.append(" ");
    String code[] = sb.toString().split(" ");
    ParseString ps = new ParseString(code);
    ps.tokenize();
    System.out.println(ps.eval());



  }
  }catch(Exception e){
    System.out.println("Error " + e.getMessage());
  }

  scanner.close();

  }




}
