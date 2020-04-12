import express, { Request, Response } from 'express';
import bodyParser from 'body-parser';
import mailgunApi from 'mailgun-js';

interface Email {
    from: string;
    to: string;
    subject: string;
    html: string;
}

const handler = (req: Request, res: Response) => {
    const apiKey = '<provide_by_env_var>';
    const domain = '<provide_by_env_var>.mailgun.org';
    const mailgun = new mailgunApi({ apiKey, domain });

    const { from, to, subject, html } = req.body as Email;
    // const data = {
    //     from: email,
    //     to: 'Liss alicedeadbride@gmail.com',
    //     subject: 'Order',
    //     text: `${items.map(i => `${i.band} - ${i.album}`).join('\n')}`
    // };
    const data = {
        from,
        to,
        subject,
        html
    };

    mailgun
        .messages()
        .send(data, (error, body) =>
            error
                ? res
                      .status(error.statusCode || 500)
                      .send(error.message || error)
                : res.status(200).send(body.message)
        );
};

require('request').debug = true;

const HOST = process.env.HOST || '0.0.0.0';
const PORT = 8002;
express()
    .use(bodyParser.json({ limit: '10mb' }))
    .post('/email', handler)
    .listen(PORT, HOST, () =>
        console.log(`Server listening at http://${HOST}:${PORT}`)
    );
