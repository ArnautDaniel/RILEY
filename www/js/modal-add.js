$(function(){
   
    $("#modalButton").click(function(){
	$("#modalText").text("Add pictures to " + $(this).parent().attr("id"));
	if (!document.getElementById("modalAlive")){ $("#modalFoot").append($('<input id="modalAlive" class="btn" type="button" value="Submit Multi-Pic"/>')); };
	
});
});
