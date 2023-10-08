ls -p | grep -v / | sed 'p;s/[A-Z]/\L&/g' | xargs -n 2 -d '\n' mv --
