FROM swipl

WORKDIR /usr/src/app

COPY . .

EXPOSE 5000

CMD swipl -s main.pl -g start
