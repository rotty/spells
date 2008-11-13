(testeez "With vectors"
  (test/equal "string"
    (string-substitute "Welcome, {0}." '#("Bob"))
    "Welcome, Bob.")
  (test/equal "several strings"
    (string-substitute "Today is the {1} {2}, {0}" '#("2008" "Nov" "13"))
    "Today is the Nov 13, 2008"))
