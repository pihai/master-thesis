module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');
    res = {
        // status: 200, /* Defaults to 200 */
        body: "Ok"
    };
    context.done(null, res);
};