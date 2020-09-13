class GetItems {

    onSubmitForm(obj, targetObj) {
        if(!(obj.originalEvent instanceof Event))
            return false;
        if(!(obj.target instanceof HTMLFormElement))
            return false;
        
        let form = $(obj.target);
        let queryParams = form.serializeArray();

        $.get("get_items", queryParams).always(data => {
            let str = "";
            if(data instanceof Object)
                str = JSON.stringify(data);
            else
                str = data;
            $(targetObj).text(str);
        })
        return true;
    }
}

window.$get_items = new GetItems();