
require('./css');

const events = {
    loaded(ui) {

        let $contents = ui.view.$el;

        let $button = $(`<div id="sim">Run simulation</div>`);
        $button.on('click', () => {
            ui.simulate.setValue(true);
        });
        $button.appendTo($contents);
    },

    onChange_option(ui, event) {
        ui.simulate.setValue(false);
    },
};

module.exports = events;
