# **How to Build Your First GitHub Copilot Extension — Step-by-Step Guide**

## **1. Understand GitHub Copilot Extensions**

* **Types of Copilot Extensions:**

  * **Public Extensions:** Available on GitHub Marketplace, contributed by partners.
  * **Private Extensions:** You create and host yourself, for your enterprise or personal use.

* This guide focuses on **creating a private Copilot extension** using the JavaScript SDK (currently in preview).

---

## **2. Setup Development Environment**

You can develop using:

* Visual Studio Code (VS Code)
* Any other IDE supporting JavaScript development

---

## **3. Create a New Repository**

Start with an **empty GitHub repository** for your extension.

---

## **4. Initialize Node.js Project**

Open terminal in your project folder and run:

```bash
npm init -y
```

This creates a `package.json` with default values.

---

## **5. Create Your Main File**

Create an `index.js` file (or any name you prefer) for your extension's code.

---

## **6. Add `type` and `start` Script in `package.json`**

Modify `package.json`:

```json
{
  "type": "module",
  "scripts": {
    "start": "node index.js"
  }
}
```

---

## **7. Install Required Packages**

```bash
npm install @copilot-extensions/preview-sdk
npm install express
```

---

## **8. Import and Setup Basic Server**

Update your `index.js`:

```js
import http from 'http';
import { createTextEvent, createDoneEvent } from '@copilot-extensions/preview-sdk';

const server = http.createServer(async (req, res) => {
  if (req.method === 'GET') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello from Copilot extension server!\n');
  } else {
    res.write(createTextEvent('Hello world from Amdocs!\n'));
    res.write(createTextEvent('This is my first copilot extension'));
    res.end(createDoneEvent());
  }
});

const PORT = 3000;
server.listen(PORT, () => {
  console.log(`Copilot extension server running on port ${PORT}`);
});
```

---

## **9. start Your Server**

Run in terminal:

```bash
npm start
```

## **Setup ngrok to Expose Localhost**

* Download and install [ngrok](https://ngrok.com/download).
* follow the setup instructions on the ngrok website.
* Run in terminal:

```bash
ngrok http 3000
```

---

## **11. Register Your Copilot Extension in GitHub**

* Go to **GitHub > Settings > Developer Settings > GitHub Apps > New GitHub App**.

Fill in:

* **App name:** e.g. `amdocs-hello`
* **Homepage URL:** your public URL or `http://github.com`
* **Callback URL:** same public URL `http://github.com`
* **Disable** - Expire user authorization tokens
* **Webhook** - Deactivate
* **Account Permissions:**

  * Under **Copilot Chat**, select **Read-only** permission (mandatory).

* **Where can this GitHub App be installed?** `Optional`

  - Only on this account
  - Any account
  
* **Create the app.**

---

## **12. Configure the Extension in GitHub Copilot Chat**

* Once the app is created, it will redirect to amdocs-hello app page.
* Select **Copilot** on the left sidebar.
* Change **App Type** to `Agent`.
* Under **Agent Definition**, add `url` with the value as your **ngrok URL** (e.g., `https://abcd1234.ngrok.io`).
* Save changes.

## **13. Make the Extension Publicly Available**

* On the same app page, click on **Advanced**.
* select **Make public** and confirm ( **Make this Github App public?** ).

## **14. Test the Extension**

* Open **Chat with Copilot** in Github portal.
* Type the extension name - `@amdocs-hello`.
* For the first time you will be prompted to authorize the app.
* After authorization, you should see the messages from your extension.
* Again open the **Chat with Copilot** and type `@amdocs-hello` to see the extension in action.

![images](../images/ghc-48.png)

* You can verify the extension on VS Code as well.
* Open VS Code and ensure you have the **GitHub Copilot** extension installed.
* Open a new file, type `@amdocs-hello` and hit `Enter`.

---