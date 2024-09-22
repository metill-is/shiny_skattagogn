box::use(
    app / main
)

function(input, output, session) {
    main$server("main")
}
