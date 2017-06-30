$(function modalSwitch(item) {
    $("[id='" + item + "']").first().click(function(event){
	event.preventDefault();
	alert("Hello");
    });	
});
