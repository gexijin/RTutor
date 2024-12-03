#____________________________________________________________________________
#  RTutor Policies
#____________________________________________________________________________


mod_17_policies_ui <- function(id) {

  ns <- NS(id)
  tagList(
    # Policies (hidden)
    conditionalPanel(
      condition = "false",
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex;justify-content: space-between;",
            # privacy policy
            actionLink("ppolicy", "Privacy Policy"),
            # terms of use
            actionLink("tofu", "Terms of Use")
          )
        )
      )
    ),

    # Footer links to policies tabs redirect
    tags$script(HTML("
    /* Update the active tab to hidden privacy policy tab */
      $(document).on('click', '#ppolicy', function() {
        $('#tabs a[data-value=\"privacy_policy\"]').tab('show');
      });
    ")),

    tags$script(HTML("
    /* Update the active tab to hidden terms of use tab */
      $(document).on('click', '#tofu', function() {
        $('#tabs a[data-value=\"terms_of_use\"]').tab('show');
      });
    "))
  )

}


mod_17_policies_serv <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Hide policy tabs from tab panel
    observe({
      shinyjs::hideElement(selector = "#tabs li a[data-value=privacy_policy]")
      shinyjs::hideElement(selector = "#tabs li a[data-value=terms_of_use]")
    })

    # Redirect to privacy policy hidden tab
    observeEvent(input$ppolicy, {
      updateNavbarPage(session, inputId = "tabs", selected = "privacy_policy")
      shiny::removeModal()
    })
    # Redirect to terms of use hidden tab
    observeEvent(input$tofu, {
      updateNavbarPage(session, inputId = "tabs", selected = "terms_of_use")
      shiny::removeModal()
    })


    # Pop-up to warn users under age 13 & those associated with a
    # for-profit organization to NOT use RTutor without a license
    observe({
      commercial_use_modal <- shiny::modalDialog(
        title = "RTutor Usage Policy",

        tags$br(),
        tags$h4("RTutor is available to the public strictly for education and
          non-profit organizations. If you are affiliated with a company or intend
          to use RTutor for commercial activities, you must obtain a license from us.
          Please contact us at ",
          a("ge@orditus.com.", href = "mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com")
        ),

        tags$br(),
        tags$h4("We've updated our ",
          actionLink("ppolicy", "Privacy Policy"),
          "and ",
          actionLink("tofu", "Terms of Use."),
          " By continuing to RTutor.ai, you acknowledge and agree to these changes."
        ),

        footer = tagList(modalButton("Agree")),

        easyClose = TRUE,
        size = "l"
      )

      shiny::showModal(commercial_use_modal)
    })

  })
}


### Policies' Content ###
terms_of_use_content <- function() {
  HTML('
    <div class="policy">
      <h1>Terms of Service</h1>
      <p style="font-weight: bold;">Updated Aug. 19th, 2024</p>
      <p>Welcome to Orditus LLC ("we", "us", or "our"). These Terms of Use ("Terms") govern your use of our website RTutor.ai and any services, content, or features provided by us (collectively, the "Services"). By accessing or using our Services, you agree to be bound by these Terms. If you do not agree to these Terms, you may not use our Services.</p>
      <h2>1. Description of Services</h2>
      <p>RTutor is designed to generate and run R and Python code. Using the power of ChatGPT, RTutor translates your natural language into scripts to analyze and interpret data.</p>
      <h2>2. Acceptance of Terms</h2>
      <p>By accessing and using our Services, you accept and agree to be bound by these Terms and our Privacy Policy. If you do not agree to these Terms, you should not use our Services.</p>
      <h2>3. Changes to Terms</h2>
      <p>We reserve the right to modify these Terms at any time. Any changes will be effective immediately upon posting of the revised Terms. Your continued use of the Services after the posting of any changes constitutes your acceptance of the revised Terms.</p>
      <h2>4. User Obligations and Restrictions</h2>
      <p>You agree to use the Services only for lawful purposes and in a manner that does not infringe the rights of, restrict, or inhibit anyone else\'s use and enjoyment of the Services. Prohibited behavior and use includes but is not limited to:</p>
      <p>You must be at least 13 years old to use this Service.<br>
      You must not use this Service commercially in any form without our written consent.<br>
      You agree not to transmit obscene or offensive content.<br>
      You agree not to use this Service to generate content that is misleading, harmful, or illegal.<br>
      You must not overuse the Services in a manner that adversely impacts the performance or availability of the Services for other users.<br>
      You must not upload data that is confidential or personally identifiable without proper anonymization.<br>
      You agree not to attempt to reverse-engineer or otherwise tamper with the AI models and algorithms.<br>
      You must not copy or scrape the content of the Service in any form without our written consent.</p>
      <h2>5. User Content</h2>
      <p>By submitting, posting, or displaying content, you grant us a worldwide, non-exclusive, royalty-free license to use, reproduce, adapt, modify, publish, and distribute such content in any and all media. You are responsible for any data you submit to our Services. By submitting data, you agree that:<br><br>
      You have the necessary rights and permissions to share the data.<br>
      You will not submit any data that violates the rights of others or any applicable laws.<br>
      We may use, store, and process basic information about your data as described in our Privacy Policy.</p>
      <h2>6. Intellectual Property</h2>
      <p>All content, trademarks, and data on this website, including but not limited to software, databases, text, graphics, icons, hyperlinks, private information, designs, and agreements, are the property of or licensed to Orditus LLC and as such are protected from infringement by local and international legislation and treaties. All content, AI models, algorithms, and data used or generated by our Services are the property of Orditus LLC or its licensors and are protected by intellectual property laws. By using our Services, you agree that:
      <br>You retain ownership of the data you submit to the AI assistant.
      <br>You grant us a license to use, reproduce, adapt, and modify your inputs as necessary to provide and improve our Services.
      <br>We retain ownership of all models, algorithms, and enhancements made to the Services based on aggregated and anonymized user data.</p>
      <h2>7. Limitation of Liability</h2>
      <p>To the fullest extent permitted by law, Orditus LLC shall not be liable for any indirect, incidental, special, consequential, or punitive damages arising from or related to your use of the Services, including but not limited to:<br><br>
      <br>Inaccuracies or errors in AI-generated results.
      <br>Loss or corruption of data.
      <br>Misuse of AI-generated insights for decision-making.
      <br>Unauthorized access to or use of your account or data.<br>
      In no event shall Orditus LLC nor its directors, employees, partners, or affiliates, be liable for any indirect, incidental, special, consequential, or punitive damages, including without limitation, loss of profits, data, use, goodwill, or other intangible losses, resulting from:<br>
      Your use or inability to use the Services.<br>
      Any unauthorized access to or use of our servers and/or any personal information stored therein.<br>
      Any breaks or stoppages in our service that affect the sending or receiving of data to or from the Services.</p>
      <h2>8. Termination</h2>
      <p>We may terminate or suspend your access to our Services immediately, without prior notice or liability, for any reason whatsoever, including without limitation if you breach the Terms. Upon termination, your right to use the Services will immediately cease.</p>
      <h2>9. Privacy Policy</h2>
      <p>Your privacy is important to us. Please review our Privacy Policy to understand how we collect, use, and protect your information. We may use this information to improve the Service. By using our Services, you consent to the data practices described in our Privacy Policy.</p>
      <h2>10. Governing Law</h2>
      <p>These Terms shall be governed and construed in accordance with the laws of South Dakota, USA without regard to its conflict of law provisions.</p>
      <h2>11. Contact Us</h2>
      <p style="padding-bottom: 90px;">If you have any questions about these Terms, please contact us at <a href="mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com">ge@orditus.com</a>.</p>
    </div>
  ')
}

privacy_policy_content <- function() {
  HTML('
    <div class="policy">
      <h1>Privacy Policy</h1>
      <p style="font-weight: bold;">Updated Aug. 19th, 2024</p>
      <p>We are committed to protecting your privacy. This Privacy Policy explains how ("we", "us", or "our"), Orditus LLC, collects, uses, discloses, and safeguards your information when you use our website RTutor.ai and our services (collectively, the "Services"). By accessing or using the Services, you agree to the terms of this Privacy Policy. If you do not agree with the terms of this Privacy Policy, please do not access or use the Services.</p>
      <h2>1. Information We Collect</h2>
      <h3>1.1 Personal Information</h3>
      <p>We may collect personal information that you provide directly to us when you use our Services or communicate with us. This information may include:<br><br>
      Name<br>
      Email address<br>
      Company name<br><br></p>
      <h3>1.2 Usage Information</h3>
      <p>We may collect information from third parties that your browser sends whenever you visit our website or use our Services. This data may include:<br><br>
      Browser type,<br>
      Browser version,<br>
      Pages of our Service that you visit,<br>
      City of user,<br>
      Time and date of your visit,<br>
      Time spent on those pages,<br>
      Other diagnostic data.<br><br>
      We do not collect IP addresses or demographic data.</p>
      <h3>1.3 Upload Information and Prompts</h3>
      <p>When you upload data to our Services, it is stored temporarily for the duration of your session. Once your session ends or you close your browser, all uploaded data is automatically deleted. We do not track or store any of the data you upload.
      <br>By default, none of your uploaded data is sent to the selected language learning model. However, if you choose, there is a settings option that allows our Services to randomly select at most five rows from your data to send to the selected Large Language Model (LLM) to enhance coding results.
      <br>We do collect data structure information and the prompts you send while using our Services. If you prefer not to share this information, you can opt out through the settings.
      <br>Additionally, column names and data type information from your uploaded data are sent to the selected language learning model to improve the functionality of the Services. This includes category names of all factor variables within your uploaded data.</p>
      <h2>2. How We Collect This Data</h2>
      <p>To collect data structure information and prompts, we utilize Google Analytics and an SQL database. Google Analytics helps us track and analyze user interactions with our Services, providing insights into how data is structured, and which prompts are most commonly used. This information is collected in real-time and stored securely in our SQLite database. The SQLite database allows us to manage and retrieve this data efficiently, ensuring that it is used to improve the quality and functionality of our Services.</p>
      <h2>3. Why We Collect This Data</h2>
      <p>We may use information from users for the following purposes:<br><br>
      To operate and improve the Services by providing you with more effective customer service.<br>
      To perform research and analysis aimed at improving the Services, or other products and technologies of Orditus LLC.<br>
      To diagnose or fix problems with the Services.<br>
      To communicate with you, especially through the sending of emails, information on technical Service issues, security announcements, information on new Services available, legal notices, response to your requests, or other information that we think might be relevant to you.<br>
      To protect against harm to the rights, property or safety of Orditus LLC, our users, yourself, or the public.<br>
      To enforce any terms of use, including investigations of potential violations of the terms.<br>
      To comply with any law, regulation, legal process, or governmental requests.</p>
      <h2>4. How We Use This Data</h2>
      <p>Orditus LLC will use the information collected from you to pursue legitimate interests such as legal or regulatory compliance, security control, business operations, scientific research, or any other interest reasonably held as legitimate.</p>
      <h2>5. Sharing This Data</h2>
      <p>We will not lease, sell, or rent your personal, usage, or upload information except in the following instances described in this policy.<br></p>
      <h3>5.1 Third Party Service Providers</h3>
      <p>We may occasionally hire other companies to provide limited services on our behalf, such as providing customer support, hosting websites, or performing statistical analysis of our Services. Those companies will be allowed to obtain only the information they need to deliver the relevant service. They will be required to maintain the confidentiality of the information and are prohibited from using it for any other purpose.<br><br>
      Third Party Service Providers:<br>
      Google Analytics<br>
      SQLite<br>
      AWS<br></p>
      <h3>5.2 With your Consent</h3>
      <p>At any time during your use of our Services, or upon explicit request from us, you may consent to the disclosure of your information.<br></p>
      <h3>5.3 For Security and Safety Purposes</h3>
      <p>In the event of any fraud, security threats, or incidents, we reserve the right to disclose your information without your consent for the purposes of maintaining security on our website and for our users and addressing fraud or security issues. We reserve the right to disclose your information without your consent for the purpose of protecting against harm to the rights, property, or safety of Orditus LLC, our users, yourself, or the public.<br></p>
      <h3>5.4 For Legal or Regulatory Purposes</h3>
      <p>We reserve the right to disclose your information without your consent to comply with any applicable law, regulation, legal process, or governmental requests.</p>
      <h2>6. Opting Out of Sharing Your Data</h2>
      <h3>6.1 Access your Information</h3>
      <p>You may be entitled under data protection laws to access and review personal information that Orditus LLC holds related to you. You may access, modify, or delete the information we collected by controlling the content that you share or upload at any time.<br><br>
      Any other request should be addressed to: <a href="mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com">ge@orditus.com</a>. Such inquiries should be clearly marked as data protection queries, and you should indicate if the request is time sensitive.<br></p>
      <h3>6.2 Data Retention</h3>
      <p>We retain your Information for as long as necessary to deliver our Services, to comply with any applicable legal requirements, to maintain security and prevent incidents, and, in general, to pursue our legitimate interests. If you wish to request the erasure of all your personal information that we process, you may do so by sending a written request to <a href="mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com">ge@orditus.com</a>.</p>
      <h3>6.3 Opting Out</h3>
      <p>You may opt out of sending information relevant to your data uploads and prompt requests at any time by switching off the checkbox in the RTutor.ai settings.</p>
      <h2>7. Data Security</h2>
      <p>All interactions with our Services occur over HTTPS which ensures that data transmitted between your device and our servers is encrypted and secure.</p>
      <h2>8. Children’s Privacy</h2>
      <p>Orditus.com is neither directed to nor structured to attract children under the age of 13 years. Accordingly, Orditus LLC does not intend to collect personal information from anyone it knows to be under 13 years of age. Orditus LLC will direct potential users under 13 years of age not to use the Services. If the Company learns that personal information of persons less than 13 years of age has been collected without verifiable parental consent, Orditus LLC will take the appropriate steps to delete this information.<br><br>
      To make such a request, please contact Orditus LLC at: <a href="mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com">ge@orditus.com</a>.</p>
      <h2>9. Contact Us</h2>
      <p style="padding-bottom: 90px;">If you have questions about this policy, please email us at: <a href="mailto:ge@orditus.com?Subject=RTutor&cc=daniel.burkhalter@orditus.com,jenna@orditus.com">ge@orditus.com</a>.</p>
  </div>
  ')
}
