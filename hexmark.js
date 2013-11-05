function region_click()
{
    var indices = this.id.split('_');
    var visibleClass = 'visible_info';
    var i;

    $('.visible_info').removeClass(visibleClass)
                      .addClass('hidden');

    var ids = [];
    // starting at 1 because the first element of the index
    // show something else.
    for (i = 1; i < indices.length; i++)
    {
        var istr = indices[i].toString();
        for (j = 0; j < ids.length; j++)
            ids[j] = ids[j] + '_' + istr;
        ids.push("i_" + istr);
    }

    for (i = 0; i < ids.length; i++)
    {
        $('#' + ids[i]).removeClass('hidden')
                       .addClass(visibleClass);
    }
}

$(document).ready(function() {
    $('.region').click(region_click);
});

